README pour utilisation du RVTK_DEBUG 
-------------------------------------
06 Oct. 2015 : Gildas Cambon V1
21 Sep. 2016 : Gildas Cambon V2

- INSTALLATION
================
- decompresser le repertoire TESTING_CROCO.tar.gz
- mettre RVTK_DEBUG_src sur son $HOME
- mettre RVTK_DEBUG sur son $WORKDIR
- Se placer dans $WORKDIR/RVTK_DEBUG
- Editer et renseigner create_link.sh
- Lancer ./git_process.bash apres avoir renseigné le path pour les sources
- Lancer ./create_link.sh :  ca cree les liens vers les sources.

- UTILISATION
=================
(-etre placé dans le $WORKDIR/RVTK_DEBUG)
- Editer le jobcomp_rvtk.bash et le renseigner
- Editer le rvtk_REGIONAL , choisir les clés CPP que l'on veut tester
- Editer le test_croco.sh , choisir si on fait ou pas de git pull
- On lance test_croco.sh REGIONAL (ou TESTCASES ou VORTEX)
- ...
- Ca tourne, ca tourne ....
- ....
- On regarde le log qui se trouve dans Log/Results_xxconfigxx_$DATE.git$number
     - xxconfig = REGIONAL, VORTEX ou TESTCASES
     - $number = le numero de revision du depot git master (obtenu avec git_process.bash)
- La ou il y a des bugbin : pas bon ... mais il n'y en a pas bcp on est
  content :-)

Nettoyage si necessaire :
===============
- ./clean_rvtkdir.sh
