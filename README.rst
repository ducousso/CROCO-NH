CROCO-NH: a non-hydrostatic version of CROCO
============================================

**CROCO-NH** relaxes the hydrostatic assumption in CROCO

`CROCO <https://gitlab.inria.fr/croco-ocean/croco>`__ is the regional
 circulation model formerly known as ROMS/AGRIF. It does relax the H
 assumption by integrating in time the vertical component of the
 velocity and by computing the pressure via a Poisson equation.  The
 Poisson equation is solved with a multigrid solver. The solver is
 parallelized, even for large number of cores. It handles sigma
 coordinates and realistic topographies.
 

Changes relative to CROCO
-------------------------

1. The third component of the velocity is prognosed. Because CROCO
   momentum equation is written in flux form, it is easier to prognose
   the 'vertical flux' (as opposed to the strict vertical velocity).
   Changes are in the OCEAN folder

2. The multigrid solver has been developped specifically. It sits in
   the NHMG folder. It can be compiled and tested as a standalone. See
   the *NHMG/README* and use the *NHMG/configure*.


Tips
----

- The multigrid has many tunable parameters. The user may access them 
  via the NHMG/nhmg_namlist

  - the user is advised to monitor the performances of the multigrid
    (e.g. *output_freq = 10* in the *nhmg_namelist*).

  - set *surface_neumann = .true.* in *nhmg_namelist* this will set a
    Neuman boundary condition (BC) type for pressure at surface. Note
    that the proper BC is of Dirichlet type but activating it imposes
    to shorten the time step, thus making the NH code much more
    computationally expensive.
    
- A NH experiment should take roughly 120% more time than a
    Hydrostatic version

- A NH experiment can be run with the exact same time step than the
  Hydro version

- The multigrid imposes some constraint on the grid size and the
  number of cores that can be used

  - grid dimensions (nx, ny, nz) should be in the form 2**p or 3*2**p
  - number of cores in x and y should be in the form 2**p

- The NH physics is activated with the cpp key *NHMG* (see Run/cppdefs.h)


Announcement
------------

A `workshop <https://nhom-brest.sciencesconf.org/>`__ *on Non-Hydrostatic​ ​ Ocean​ ​ Modeling* is organized in Brest (France) in October 2018.

 
