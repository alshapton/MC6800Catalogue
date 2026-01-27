:orphan:

.. _family:

The Motorola MC6800 Family
==========================

Starting with the introduction of the MC6800 in 1974, Motorola constantly expanded and improved the family.

Some of these family of processors were incompatible with the core MC6800 chip, and are not considered part of the scope of interest of this collection.

The processors of interest together with their broad family characteristics are:

.. include:: ../../xbuild_support/familyfile.inc

.. Note::
      "6801","NMOS","-","-","-","72","Yes","-"
      "6802","NMOS","128","-","-","72","Yes","-"
      "6802NS","NMOS","128","-","-","72","Yes","-"
      "6803","HMOS","128","13","Serial","82","Yes","16-bit"
      "6803NR","HMOS","-","13","Serial","82","Yes","16-bit"
      "6803E","HMOS","128","13","Serial","82","-","16-bit"
      "6808","HMOS","-","-","-","72","-","Yes"
      "6809","HMOS","-","-","-","59","-","Yes"
      "6809E","HMOS","-","-","-","59","-","-"

.. Note:: 
   - Some mnemonic instructions can have many opcode instructions. As a result, a microprocessor has many more opcode instructions than mnemonic instructions. For instance, the MC6809 has 59 mnemonic instructions and 1464 opcode instructions.
   - All microprocessors have a maximum limit of 64K of addressable memory, except for the MC6809 and MC609E when used in conjunction with the MC6829 Memory Management Unit, which increases the limit to 2038K (or 2Mb).
   - All microprocessors have 40 pins.
   - All microprocessors have a data bus size of 8 bits.
