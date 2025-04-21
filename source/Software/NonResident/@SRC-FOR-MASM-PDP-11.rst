:orphan:

.. _SRC-FOR-MASM-PDP-11:

Motorola FORTRAN Source Code for MSAM68 Cross Assembler (PDP-11)
================================================================

.. rubric:: The source presented here is believed to be incomplete. There are references to files which did not come with this source, and appear not to be part of the PDP-11 FORTRAN support files. It is doubtful that this would compile.
                        
.. code-block:: 

      FORTRAN SOURCE FOR MOTOROLA'S M68SAM-11-1.2               
            COPYRIGHT 1974 AND 1975                         

      PROGRAM MPAM                                                         
      NAM:  MPAM    VER: 1.0  DAT: 09-10-75  CMP: PDP-11       
      PGM:  MAIN ROUTINE FOR SYSTEM M68SAM                     
                                                             
      SYS:  M68SAM                                             
                                                             
      FNC:  THIS IS THE MAIN PROGRAM OF M68SAM, IT INITIALIZES 
            ALL LABELED AND UNLABELED COMMON AND CALLS MPAM0.  

      RT-11 ADDITIONS MADE BY STEVE WILLIAMS, UT 20-OCT-76
      
      FILES REQUIRED:
         RK0:
            FORTRA.SAV
            LINK.SAV
            FORLIB.OBJ
         RK1:
            ASM.FOR
            MPVMUL.MAC
            MPVDIV.MAC
            M6800.HLP

:download:`Download <../../_static/Software/6800_asm.pdp-11.for>`

.. rubric:: Collection Information

.. csv-table:: 
   :header: "Acquired"
   :widths: auto

   :material-regular:`verified;2em;sd-text-success` 21-APR-2025


