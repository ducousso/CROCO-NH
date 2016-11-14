#!/usr/bin/env python

"""Generate a dot graph from a list of Fortran 90 files."""

__author__ = "Marc Honnorat"
__version__ = "0.1.1"

import sys
import os.path
import re
import math
import optparse
import fileinput
import string

debug = 1
quiet = 1
warns = 0
prune = 0

def _print(str,fi=None):
	if quiet: return
	if fi:	sys.stderr.write("%40s: l.%4d: " % (fi.filename(), fi.filelineno()) + str)
	else:	sys.stderr.write(str+'\n')

class Function:
	"""A function."""
	def __init__(self, type, name, module_name = None, file_name = None, re = None):
		self.type = type
		self.name = name
		self.id   = type + "_" + name
		if module_name:
			self.id = self.id + "_MOD_" + module_name
		self.module = module_name
		self.file = file_name
		self.re = re
		self.calls   = {}  # functions that are called by self.
		self.callers = {}  # functions that call self.
	
	def add_call_to(self, callee):
		self.calls[callee.id] = callee

	def __repr__(self):
		return self.name

class Tree:
	"""The whole profile."""

	def __init__(self):
		self.functions = {}
		self.modules = {}

	def add_function(self, function):
		if function.id in self.functions:
			if warns: sys.stderr.write('warning: overwriting function %s (id %s)\n' % (function.name, str(function.id)))
		self.functions[function.id] = function

	def add_module(self, module_name, file_name=None):
		if module_name in self.modules:
			if warns: sys.stderr.write('warning: overwriting module %s\n' % (module_name))
		self.modules[module_name] = file_name

	def find_function(self, function_name):
		for function in self.functions.itervalues():
			if function_name.lower() == function.name.lower(): return function
		return None

	def validate(self):
		"""Validate the edges."""
		for function in self.functions.itervalues():
			for callee_id in function.calls.keys():
				if callee_id not in self.functions:
					if warns: sys.stderr.write('warning: call to undefined function %s from function %s\n' % (callee_id, function.name))
					del function.calls[callee_id]
			for callee_id in function.calls.keys():
				caller_id = function.id
				self.functions[callee_id].callers[caller_id] = function
		if debug:
			for function in self.functions.itervalues():
				sys.stderr.write('FUNCTION '+function.name+' is called by : '+str(function.callers.values())+'\n')
		if prune: self.prune_singletons()

	def prune_modules(self, modules_list):
		for function in self.functions.values():
			for module_name in modules_list:
				if function.module and function.module.lower() == module_name.lower():
					if debug: sys.stderr.write('pruning module '+module_name+' -> function '+function.name+'\n')
					for callee_id in function.calls.keys():
						del self.functions[callee_id].callers[function.id]
					for caller_id in function.callers.keys():
						del self.functions[caller_id].calls[function.id]
					del self.functions[function.id]
		
	def prune_singletons(self):
		for function in self.functions.values():
			if len(function.calls) == 0 and len(function.callers) == 0:
				if debug: sys.stderr.write('pruning function '+function.module+'::'+function.name+' (no caller, no callee)\n')
				del self.functions[function.id]
			if len(function.calls) == 1 and len(function.callers) == 1:
				if function.calls.keys()[0] == function.callers.keys()[0]:
					if debug: sys.stderr.write('pruning recursive function '+function.module+'::'+function.name+' (no caller, no callee)\n')
					del self.functions[function.id]

	def dump(self):
		for function in self.functions.itervalues():
			sys.stderr.write('%s:\n' % (function.name,))
			for callee in function.calls.itervalues():
				sys.stderr.write(' - calls %s:\n' % (callee,))

class Theme:

    def __init__(self, 
            fgcolor = "#000000",
            bgcolor = "#FFFFFF",
            fontname = "Courier",
            fontsize = 14.0,
            penwidth = 1.5,
            ranksep = 1.5,
            nodesep = 0.125
            ):
        self.bgcolor = bgcolor
        self.fgcolor = fgcolor
        self.fontname = fontname
        self.fontsize = fontsize
        self.penwidth = penwidth
        self.ranksep = ranksep
        self.nodesep = nodesep

BW_COLORMAP = Theme(
    fontsize = 36.0,
    penwidth = 2.0,
    ranksep  = "0.5 equally",
    nodesep  = 0.5
)
	
class DotWriter:
	"""Writer for the DOT language.

	See also:
	- "The DOT Language" specification
	  http://www.graphviz.org/doc/info/lang.html
	"""

	def __init__(self, fp):
		self.fp = fp

	def graph(self, profile, theme):
		self.write('digraph {\n')
		self.attr('graph', fontname=theme.fontname, ranksep=theme.ranksep, nodesep=theme.nodesep, concentrate='true', rankdir='LR')
		self.attr('node', fontname=theme.fontname, shape="box", style="filled", fontcolor="black", width=0, height=0)
		self.attr('edge', fontname=theme.fontname)

		for function in profile.functions.itervalues():
			labels = []
			self.node(function.id, 
				fontcolor = theme.fgcolor,
				fontsize  = "%.2f" % theme.fontsize,
				label     = "%s" % function.name
			)

			for callee in function.calls.itervalues():
				labels = []
				self.edge(function.id, callee.id, 
					color = theme.fgcolor, 
					fontcolor = theme.fgcolor,
					fontsize = "%.2f" % theme.fontsize, 
					penwidth = "%.2f" % theme.penwidth, 
					labeldistance = "%.2f" % theme.penwidth, 
					arrowsize = "%.2f" % (math.sqrt(theme.penwidth))
				)

		for module_name in profile.modules.iterkeys():
			self.write('\tsubgraph cluster_module_%s {\n' % module_name)
			label = module_name
#			if profile.modules[module_name]:
#				label = label + " (in "+profile.modules[module_name]+")"
			self.attr( '\tgraph', label=label, style="filled", fillcolor="grey60", fontsize="%.2f" % (theme.fontsize*1.4),
								  labeljust='l')
			for function in profile.functions.itervalues():
				if function.module == module_name:
					self.write('\t\t%s\n' % function.id)
			self.write('\t}\n')
		
		self.write('}\n')

	def attr(self, what, **attrs):
		self.write("\t")
		self.write(what)
		self.attr_list(attrs)
		self.write(";\n")

	def node(self, node, **attrs):
		self.write("\t")
		self.write(node)
		self.attr_list(attrs)
		self.write(";\n")

	def edge(self, src, dst, **attrs):
		self.write("\t")
		self.write(src)
		self.write(" -> ")
		self.write(dst)
		self.attr_list(attrs)
		self.write(";\n")

	def attr_list(self, attrs):
		if not attrs:
			return
		self.write(' [')
		first = True
		for name, value in attrs.iteritems():
			if first:
				first = False
			else:
				self.write(", ")
			self.write(name)
			self.write('=')
			self.write(self.escape(str(value)))
		self.write(']')

	def escape(self, s):
		s = s.encode('utf-8')
		s = s.replace('\\', r'\\')
		s = s.replace('\n', r'\n')
		s = s.replace('\t', r'\t')
		s = s.replace('"', r'\"')
		return '"' + s + '"'

	def write(self, s):
		self.fp.write(s)

class TextWriter:
	"""Write in ASCII"""

	def __init__(self, fp):
		self.fp = fp
	
	def graph(self, tree, theme):
		for function in tree.functions.itervalues():
			self.fp.write("%s %s:\n" % (function.type, function.name))
			for callee in function.calls.itervalues():
				self.fp.write(" - calls %s %s\n" % (callee.type, callee.name))
			for caller in function.callers.itervalues():
				self.fp.write(" - called by %s %s\n" % (caller.type, caller.name))

re_commentline = re.compile(r'^\s*!.*')
re_cppkey      = re.compile(r'^\s*#.*')
re_cont1 = re.compile(r'(?P<line>.*)&\s*\Z')
re_cont2 = re.compile(r'(\s*&|)(?P<line>.*)')

re_module_proc_def   = re.compile(r'^\s*module\s+procedure\s+(?P<names>([a-z]+\w*,?\s*)+[a-z]+\w*)',re.I)
re_module_name       = re.compile(r'^\s*module\s+(?P<name>[a-z]+\w*)\s*',re.I)
re_interface_def     = re.compile(r'^\s*interface\s+(?P<name>[a-z]+\w*)\s*',re.I)
re_unnamed_interface = re.compile(r'^\s*interface\s*\Z', re.I)
re_program_name      = re.compile(r'^\s*program\s*(?P<name>[a-z]+\w*)\s*.*',re.I)
re_subroutine_name   = re.compile(r'^\s*(recursive)?\s*subroutine\s*(?P<name>[a-z]+\w*)\s*.*',re.I)
re_function_name     = re.compile(r'^\s*([a-z]+[\w\s]*\s)?\s*function\s*(?P<name>[a-z]+\w*)\s*',re.I)
re_end_statement     = re.compile(r'^\s*end\s*(subroutine|function|program).*',re.I)
re_end_module        = re.compile(r'^\s*end\s+module\s+.*',re.I)
re_end_interface     = re.compile(r'^\s*end\s+interface\s+.*',re.I)
re_call_subroutine   = re.compile(r'.*call\s+(?P<name>[a-z]+\w*).*',re.I)

class F90Parser:
	""" Parser for Fortan 90 (free-form only) """

	def __init__(self, inputs):
		self.inputs = inputs
		self.tree = Tree()
		self.in_file = None
		self.in_function = None
		self.in_module = None
		self.in_interface = None
	
	def parse(self, func=_print):
		_print("F90Parser -- first pass")
		self.read_fortran(self.crackline1)
		_print("F90Parser -- second pass")
		self.read_fortran(self.crackline2)
		_print("F90Parser -- validate tree")
		self.tree.validate()
		return self.tree
		
	def read_fortran(self, func=_print):
		""" Scan each line of file list 'self.inputs', removes comments, expand continuation lines
			and apply 'func' to each resulting line.
		"""
		cont=0
		oldcont=0
		finalline=''
		ll=''
		fi = fileinput.FileInput(self.inputs)
		for line in fi:
			if not line: break
			line = line.expandtabs().replace('\xa0',' ')
			if re_commentline.match(line): continue
			if re_cppkey.match(line): continue
			r = re_cont1.match(line)
			if r: line = r.group('line').strip() # Continuation follows ..
			if cont:
				ll = ll + re_cont2.match(line).group('line').strip()
				finalline=''
				oldcont=1
			else:
				finalline = ll
				ll = line
				if oldcont:
					finalline = finalline+'\n'
					oldcont=0
			cont=(r is not None)
			if (finalline):
				finalline = re.sub(r'\'.*\'',r'',finalline)		# removes text included in 'quotes'
				finalline = re.sub(r'".*"',r'',finalline)		# removes text included in "quotes"
				finalline = re.sub(r'!.*$',r'',finalline)		# removes trailing ! comments
				func(finalline,fi)
	
	def crackline1(self, line, fi=None):
		""" Called for each 'line' during the first pass.
			Looks for definitions of modules, subroutines and functions.
		"""
		if re_unnamed_interface.match(line):
			self.in_interface = 1
			return
		if re_end_interface.match(line):
			self.in_interface = None
			return
		if (self.in_interface == 1):
			return
		if re_end_module.match(line):
			self.in_module = None
			return
		
		self.in_file = os.path.basename(fi.filename())

		if self.in_interface:
			m = re_module_proc_def.match(line)
			if m:
				for f in m.group('names').replace(' ','').split(','):
					ff = Function("subroutine", f.strip(), self.in_module, self.in_file)
					self.in_interface.add_call_to(ff)
					self.tree.add_function(ff)
					if debug: _print("    - module procedure : "+f+'\n', fi)
				return

		m = re_interface_def.match(line)
		if m:
			interface_name = m.group('name')
			self.in_interface = Function("subroutine", interface_name, str(self.in_module), self.in_file)
			self.tree.add_function(self.in_interface)
			if debug: _print(" -- interface  : "+interface_name+' (in '+str(self.in_module)+')\n', fi)
			return
		
		m = re_subroutine_name.match(line)
		if m and not re_end_statement.match(line):
			subroutine_name = m.group('name')
			if debug: _print(" -- subroutine : "+subroutine_name+' (in '+str(self.in_module)+')\n', fi)
			self.tree.add_function(Function("subroutine", subroutine_name, str(self.in_module), self.in_file))
			return
		
		m = re_function_name.match(line)
		if m and not re_end_statement.match(line):
			function_name = m.group('name')
			if debug: _print(" -- function   : "+function_name+' (in '+str(self.in_module)+')\n', fi)
			re_f = re.compile(r".*(\W%s\s*\().*" % function_name) # il y a moyen de mieux faire...
			self.tree.add_function(Function("function", function_name, self.in_module, self.in_file, re_f))
			return

		m = re_module_name.match(line)
		if m and not re_end_statement.match(line):
			self.in_module = m.group('name')
			self.tree.add_module(self.in_module, self.in_file)
			if debug: _print("## module : === "+m.group('name')+' ===\n', fi)
			return

		m = re_program_name.match(line)
		if m and not re_end_statement.match(line):
			if debug: _print(" -- program : "+m.group('name')+'\n', fi)
			self.tree.add_function(Function("program", m.group('name'), self.in_module, self.in_file))
			return
			
	
	def crackline2(self, line, fi=None):
		""" Called for each 'line' during the second pass.
			Looks for calls to subroutines and functions.
		"""
		if re_unnamed_interface.match(line): self.in_interface = 1
		if re_interface_def.match(line): self.in_interface = 1
		if re_end_interface.match(line): self.in_interface = None
		if (self.in_interface == 1): return
		if re_end_module.match(line):
			if debug: _print(" # Leaving module   : "+str(self.in_module)+'\n', fi)
			self.in_module = None
			return
			
		# Look if we leave a block (subroutine, function, program)
		if re_end_statement.match(line):
			if debug: _print("     ... leaving "+str(self.in_function)+'\n', fi)
			self.in_function = None
			return
		
		# Look if we step into a module
		m = re_module_name.match(line)
		if m and not re_end_statement.match(line):
			self.in_module = m.group('name')
			if debug: _print(" # Entering module  : "+m.group('name')+'\n', fi)
			return

		# Look if we step into a program
		m = re_program_name.match(line)
		if m and not re_end_statement.match(line):
			self.in_function = self.tree.find_function(m.group('name'))
			if self.in_function is None:
				if warns: _print("     ... warning: unrecognized program "+m.group('name')+'\n', fi)
				return
			if debug: _print(" -- in program    : "+str(self.in_function)+'\n', fi)
			return
		
		# Look if we step into a subroutine definition
		m = re_subroutine_name.match(line)
		if m and not re_end_statement.match(line):
			self.in_function = self.tree.find_function(m.group('name'))
			if self.in_function is None:
				if warns: _print("     ... warning: unrecognized subroutine "+m.group('name')+'\n', fi)
				return
			if debug: _print(" -- in subroutine : "+str(self.in_function)+' (in module '+str(self.in_module)+')\n', fi)
			return

		# Look if we step into a function definition
		m = re_function_name.match(line)
		if m and not re_end_statement.match(line):
			self.in_function = self.tree.find_function(m.group('name'))
			if self.in_function is None:
				if warns: _print("     ... warning: unrecognized function "+m.group('name')+'\n', fi)
				return
			if debug: _print(" -- in function   : "+str(self.in_function)+'\n', fi)
			return
		
		# Look for subroutine calls
		m = re_call_subroutine.match(line)
		if m and self.in_function is not None:
			f = self.tree.find_function(m.group('name'))
			if f is None:
				if warns: _print("     ... warning: call to "+m.group('name')+' not followed\n', fi)
				return
			self.in_function.add_call_to(f)
			if debug: _print("   --> calling "+m.group('name')+" -- "+f.id+'\n', fi)
		
		# Look for function calls
		if self.in_function is not None:
			for f in self.tree.functions.itervalues():
				if f.type != "function": continue
				if f.re:
					fres = f.re.match(line)
					if fres:
						if debug: _print("   --> calling "+f.name+" -- "+f.id+'\n', fi)
						self.in_function.add_call_to(f)

class DummyParser:

	def __init__(self):
		self.tree = Tree()
	
	def parse(self):
		toto = Function("subroutine", "toto")
		tata = Function("function", "tata", "monmod")
		titi = Function("function", "titi", "monmod")
		toto.add_call_to(tata)
		toto.add_call_to(titi)
		tata.add_call_to(titi)
		tata.add_call_to(titi)
		self.tree.add_module("monmod")
		self.tree.add_function(toto)
		self.tree.add_function(tata)
		self.tree.add_function(titi)
		self.tree.add_function(tata)
		self.tree.validate()
		return self.tree

class Main:
	"""Main program."""
	
	themes = {
		"bw": BW_COLORMAP,
	}
	
	def main(self):
		"""Main program."""
		
		parser = optparse.OptionParser(
			usage="\n\t%prog [options] files",
			version="%%prog %s" % __version__)
		
		parser.add_option(
			'-o', '--output', metavar='FILE',
			type="string", dest="output",
			help="output filename [stdout]")
		
		parser.add_option(
			'-x', '--exclude-module', metavar='MODULE_LIST',
			action="append", type="string", dest="pruned_modules",
			help="list of module to exclude from the graph")
		
		parser.add_option(
			'-q', "--quiet",
			action="store_true", dest="quiet", default=False,
			help="don't print status messages to stderr")

		parser.add_option(
			'-v', "--verbose",
			action="store_true", dest="debug", default=False,
			help="print more status messages to stderr")

		parser.add_option(
			'-w', "--warnings",
			action="store_true", dest="warns", default=False,
			help="print warning messages to stderr")

		parser.add_option(
			'-p', "--prune-singletons",
			action="store_true", dest="prune", default=False,
			help="removes functions not linked to others")
		
		(self.options, self.args) = parser.parse_args(sys.argv[1:])
		
		global debug, quiet, warns, prune
		debug = self.options.debug
		quiet = self.options.quiet
		warns = self.options.warns
		prune = self.options.prune
		
		if debug: warns = True
		
		if self.options.output is None:
			self.output = sys.stdout
		else:
			self.output = open(self.options.output, 'wt')

#		self.tree = DummyParser().parse()
		self.tree = F90Parser(self.args).parse()
		
		if self.options.pruned_modules:
			pruned_modules = string.join(self.options.pruned_modules,',').split(',')
			self.tree.prune_modules(pruned_modules)
		
		self.write_graph()
	
	def write_graph(self):
		""" Write the call tree in dot format """
		if not quiet: sys.stderr.write('... writing tree to %s\n' % self.output)
#		out = TextWriter(self.output)
		out = DotWriter(self.output)
		out.graph(self.tree, self.themes["bw"])

if __name__ == '__main__':
	Main().main()