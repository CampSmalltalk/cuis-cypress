[Cuis](http://www.jvuletich.org/Cuis/Index.html) support for the Cypress package file format.

Right now we have 6 dialects each with their own unique implementation of the common Cypress package structure:

  Squeak/Pharo/GemStone all share the 
    FileTree project which reads/writes
    Cypress package structure using Monticello
    packages.
  VW uses the STIG project, which reads/writes
    Cypress package structure using VW packages.
  Cuis uses the cuis-cypress project which 
    reads/writes Cypress format using Cuis 
    packages.
  Amber uses the amber-cypress project which
    reads/writes Cypress format using Amber
    packages.


A little history (by Dale Henrichs)
----------------------------------------

In January I created the initial FileTree project based on work that Otto Behrens had done.

In March I gave a talk at the STIC 2012 conference entitled "Practical Git for Smalltalk"[1]. At the Camp Smalltalk following the conference a number of us got together and agreed upon the Cypress package structure[2]. 

Martin Kobetic began work adapting STIG[3] originally written by Travis Griggs for VisualWorks to the Cypress package structure.

In the following months I migrated the FileTree format to conform to the Cypress package structure.

At that time the two outstanding implementations of the Cypress package structure had no common code and there was no reference implementation.

I started the amber-cypress[2] with the intent of writing reference implementation of the Cypress package structure. 

When I finished that work, I started the cuis-cypress[5] project and that's when you got involved and finished up the work, quite nicely.