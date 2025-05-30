:orphan:

.. _HEPEducatorKit-II-2:


From the March 1978 issue of Radio-Electronics magazine
=======================================================

>The Motorola Educator II HEP kit IS AN M6800 microcomputer system learning tool designed with future expansion in mind. Data is entered through eight toggle switches, and the system is controlled by three additional center-off, spring-loaded toggle switches. The binary data-output display consists of eight discrete LED's. A ninth LED is extinguished when the processor is running. To power the unit an external 5-volt, 1-amp supply such as the Motorola HK/001 is required. A beginner should start out by confronting data and instructions on a bit-by-bit basis with the toggle switches. As he advances, the computerist will seek more convenient modes of operation, and can add more sophisticated keyboard and display systems through two 44-pin edge connectors.
>The Educator 11 microcomputer board uses a HEP version of the M6800 (HEP C4801), two IC's with 512 words of RAM that contain utility programs, 128 words of user RAM and a C4821 peripheral interface adapter (PIA) with 16 inputs and outputs, and some hand-shaking leads.

>The M6800 is a popular microprocessor with a versatile architecture that uses two 8-bit accumulators, a 16-bit index register, a 16-bit program counter, a 16-bit stack pointer and an 8-bit condition code register (CCR).

>The ROM firmware, DBUG, has routines for loading, reading and running user programs. It also contains a cassette tape routine. This produces an audio output and deposits specified portions of RAM into an external cassette tape recorder. A complementary read routine is intelligent enough to search a tape for a file number indicated by the data-switch settings.

>The M6800 microprocessor recognizes the software interrupt instruction (SWI) code 3F. When the microprocessor encounters this instruction, a vectored jump to DBUG saves the status of the accumulators, index register, program counter and CCR in RAM locations 00F4 through 00FA. This is an invaluable troubleshooting aid. You can, by inserting SWI instructions in crucial locations in the program, interrupt program execution at those places so that you can inspect the registers and memory contents.

>The 128 words of RAM provided with the kit are assigned addresses from 0080 to 00FF, with the last 14 locations, F2 through FF, reserved for use by the DBUG firmware. There is enough board space for an additional 128 words from 0000 to 007F. Larger blocks of memory can be added externally using one of the 44-pin connectors and proper address decoding.

>To read memory, the upper and lower bytes of the address to be inspected are entered with the data switches and the LOAD LEAST SIGNIFICANT and LOAD MOST SIGNIFICANT switches. The EXAMINE switch is then used to sequentially inspect memory locations starting at that address. Memory contents are read out in binary by the 8 data LED's. Each time you depress the EXAMINE switch, the lower byte of the address being examined is displayed on the LED readout. Releasing the switch displays the memory contents. Storing programs or data into memory is similar except that the STORE switch is used and data is entered by the data switches. Running a program is a bit more complicated since the program starting address must first be stored in the DBUG program counter storage locations 00F9 and 00FA. Then the GO/HALT switch is toggled to initiate processing.

>The manual includes kit assembly instructions, an explanation of the microcomputer hardware, some theory of number systems, suggestions on writing programs, operating instructions and application programs. A sche- matic and a firmware listing are included. However, only one of the two pages of firmware is listed, but this may be due to the early edition of the manual. The other page is easily read out using the EXAMINE facility.

>Kit assembly is straightforward, but, as with all computer kits, it requires meticulous soldering technique. The PIA with its complex control and data-direction register structure is described adequately. Each microprocessor instruction and the six addressing modes are described in detail.

>Inevitably, when you debug programs, you must add or delete instructions. Assuming there is sufficient memory space, deleting instructions can be as simple as overwriting with no-operation commands. Adding instructions, though, means moving every instruction following the added instruction. The alternate technique of inserting patches or jumps to other parts of memory is both slow and wasteful of memory. The manual presents one short application program that moves all the instructions down one memory location, starting at the location where the addition is to be made, and inserts the added instruction at that point. The program can be executed repeatedly so multiple word instructions or a series of instructions can be inserted sequentially by depressing Go as many times as required. A delete-data program works similarly, taking out one word at a time by moving all subsequent program steps up one byte and overwriting the instruction being removed. (It's a big help.)

>Educator II looks like a good way to get going with a capable microprocessor that has the capability to grow. The HEP Kit is priced at $169.95. For more information, write Motorola HEP/MRO Operations, 705 West 22nd Street, Tempe, AZ 85282.

