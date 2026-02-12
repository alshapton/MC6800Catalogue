C************************************************************              10000
C************************************************************              10005
C************************************************************              10010
C***                                                     ****              10015
C***  ARTICLES, INFORMATION AND DATA ENCLOSED HEREIN     ****              10020
C***  ARE PROPRIETARY TO MOTOROLA AND MAY NOT BE         ****              10025
C***  DISTRIBUTED, REPRODUCED OR DISCLOSED OUTSIDE       ****              10030
C***  BUYER'S ORGANIZATION WITHOUT THE EXPRESS WRITTEN   ****              10035
C***  CONSENT OR APPROVAL OF AN AUTHORIZED MOTOROLA      ****              10040
C***  OFFICER.                                           ****              10045
C***                                                     ****              10050
C************************************************************              10055
C************************************************************              10060
C************************************************************              10065
C*******                                             ********              10070
C******* FORTRAN SOURCE FOR MOTOROLA'S M68SAM-11-1.2 ********              10075
C*******           COPYRIGHT 1974 AND 1975           ********              10080
C*******                                             ********              10085
C************************************************************              10090
C************************************************************              10095
C************************************************************              10100
C     PROGRAM MPAM                                                         10105
C+    NAM: MPAM    VER: 1.0  DAT: 09-10-75  CMP: PDP-11                    10110
C     PGM: MAIN ROUTINE FOR SYSTEM M68SAM                                  10115
C                                                                          10120
C     SYS: M68SAM                                                          10125
C                                                                          10130
C     FNC: THIS IS THE MAIN PROGRAM OF M68SAM, IT INITIALIZES              10135
C          ALL LABELED AND UNLABELED COMMON AND CALLS MPAM0.               10140
C<UT>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C
C	RT-11 ADDITIONS MADE BY STEVE WILLIAMS, UT 20-OCT-76
C	
C	COMPILATION INSTRUCTIONS:
C
C	FILES REQUIRED:
C	RK0:
C	     FORTRA.SAV
C	     LINK.SAV
C	     FORLIB.OBJ
C	RK1:
C	    ASM.FOR
C	    MPVMUL.MAC
C	    MPVDIV.MAC
C	    M6800.HLP
C
C	AFTER BOOTING RT-11, TO COMPILE AND LINK THIS PROGRAM,
C	THE FOLLOWING DIALOG IS USED:
C	.ASSIGN RK1 DK
C	.R FORTRA
C	*ASM=ASM
C	*^C
C	.R MACRO
C	*MPVMUL=MPVMUL
C	*MPVDIV=MPVDIV
C	*^C
C	.R LINK
C	*RK0:M6800.SAV=MPVDIV,MPVMUL,ASM/F
C	*^C
C	.R PIP
C	*RK0:M6800.HLP=RK1:M6800.HLP
C	*^C
C
C	^
C	!
C	!---------- FIRST CHARACTER IS TYPED BY THE MACHINE
C	
C	^C MEANS CTRL-C
C 	THE RESULTING FILE IS M6800.SAV AND IS THE LOAD MODULE FOR THE
C	ASSEMBLER.  OPERATING INSTRUCTIONS MAY BE OBTAINED BY RUNNING
C	THE PROGRAM, AND ANSWERING YES TO THE HELP MESSAGE.
C
C<<<<<<<<<<<<<<<<<UT<<<<<<<<<<<<UT<<<<<<<<<
C     *************************************************                    10150
C     ***                                           ***                    10155
C     ***  COPYRIGHT 1974 AND 1975 BY MOTOROLA INC  ***                    10160
C     ***                                           ***                    10165
C     *************************************************                    10170
C*                                                                         10175
      COMMON ICOMON,LULT,LUSI,LUOT,LLSPSP,IBPWD,KCFOTB(16),KCFF            10180
      COMMON KC7F7F,KC80,ISIBUF(80),NB(121),INX,LABEL(4),LABESW            10185
      COMMON IOPCOD(4),NPNAM(4),NPGNO,IPGLOL,IPGLEN,NPLCT,ICSHF            10190
      COMMON IOTBUF(80),IOTINX,IOTSW,IOTCKS,NOTADR,NAM(5)                  10195
      COMMON ISIMBF(43),IC,NC,ICSW,IPASS,IOPCLS,IOPIXB,IOPBIN              10200
      COMMON IOPAN1,IPCT,ITERR,NRTSW,NSSCOL,NSOSCH,NAMSW,LSCM              10205
      COMMON LIMA,LDRA,LSPCT,LSASC,NOPIB,NOPT(2),NOPC(4),NOPCL             10210
      COMMON IXBASE,LSPSP,L8SP,L10,L16,LSP,LASK,LPOS,LCOMA,LMNS            10215
      COMMON LSLASH,L0,L1,L9,LA,LB,LD,LF,LH,LN,LO,LQ,LR,LS,LX              10220
      COMMON LZ,NOSYM,ISYM(813),LSYM                                       10225
      COMMON /A/ NOPCD(246),IOPBC2(51),IOPBC3(16)                          10230
      COMMON /A/ IOPBC4(116),IOPBC5(22)                                    10235
      INTEGER KCOMON(1245),IMAGE(40)                                       10240
      EQUIVALENCE (KCOMON(1),ICOMON),(N1,NAM(1)),(N2,NAM(2))               10245
      EQUIVALENCE (N3,NAM(3)),(N4,NAM(4)),(N5,NAM(5))                      10250
      EQUIVALENCE (ISILN,ISIMBF(2)),(ISIFCH,ISIMBF(3))                     10255
      EQUIVALENCE (IMAGE(1),ISIMBF(4)),(NOPBAS,NOPT(1))                    10260
      EQUIVALENCE (NOPLI,NOPT(2))                                          10265
      KCOMON(1)=1245                                                       10270
      LULT=5                                                               10275
      LUSI=1                                                               10280
      LUOT=2                                                               10285
      LLSPSP=8224                                                          10290
      IBPWD=16                                                             10295
      KCFOTB(1)=1                                                          10300
      KCFOTB(2)=2                                                          10305
      KCFOTB(3)=4                                                          10310
      KCFOTB(4)=8                                                          10315
      KCFOTB(5)=16                                                         10320
      KCFOTB(6)=32                                                         10325
      KCFOTB(7)=64                                                         10330
      KCFOTB(8)=128                                                        10335
      KCFOTB(9)=256                                                        10340
      KCFOTB(10)=512                                                       10345
      KCFOTB(11)=1024                                                      10350
      KCFOTB(12)=2048                                                      10355
      KCFOTB(13)=4096                                                      10360
      KCFOTB(14)=8192                                                      10365
      KCFOTB(15)=16384                                                     10370
      KCFF=255                                                             10375
      KC7F7F=32639                                                         10380
      KC80=128                                                             10385
      LABESW=1                                                             10390
      NPGNO=0                                                              10395
      IPGLOL=72                                                            10400
      IPGLEN=63                                                            10405
      NPLCT=200                                                            10410
      ICSHF=256                                                            10415
      ISIMBF(1)=0                                                          10420
      ISILN=0                                                              10425
      IPCT=0                                                               10430
      ITERR=0                                                              10435
      NRTSW=1                                                              10440
      NAMSW=1                                                              10445
      LSCM=42                                                              10450
      LIMA=35                                                              10455
      LDRA=0                                                               10460
      LSPCT=42                                                             10465
      LSASC=39                                                             10470
      NOPIB=10                                                             10475
      NOPBAS=16                                                            10480
      NOPLI=2                                                              10485
      NOPC(1)=17474                                                        10490
      NOPC(2)=16723                                                        10495
      NOPC(3)=19529                                                        10500
      NOPC(4)=21332                                                        10505
      NOPCL=4                                                              10510
      IXBASE=3                                                             10515
      LSPSP=8224                                                           10520
      L8SP=14368                                                           10525
      L10=12592                                                            10530
      L16=12598                                                            10535
      LSP=32                                                               10540
      LASK=42                                                              10545
      LPOS=43                                                              10550
      LCOMA=44                                                             10555
      LMNS=45                                                              10560
      LSLASH=47                                                            10565
      L0=48                                                                10570
      L1=49                                                                10575
      L9=57                                                                10580
      LA=65                                                                10585
      LB=66                                                                10590
      LD=68                                                                10595
      LF=70                                                                10600
      LH=72                                                                10605
      LN=78                                                                10610
      LO=79                                                                10615
      LQ=81                                                                10620
      LR=82                                                                10625
      LS=83                                                                10630
      LX=88                                                                10635
      LZ=90                                                                10640
      NOSYM=3                                                              10645
      ISYM(1)=16672                                                        10650
      ISYM(2)=8224                                                         10655
      ISYM(3)=8224                                                         10660
      ISYM(4)=32767                                                        10665
      ISYM(5)=16928                                                        10670
      ISYM(6)=8224                                                         10675
      ISYM(7)=8224                                                         10680
      ISYM(8)=32767                                                        10685
      ISYM(9)=22560                                                        10690
      ISYM(10)=8224                                                        10695
      ISYM(11)=8224                                                        10700
      ISYM(12)=32767                                                       10705
      ISYM(13)=0                                                           10710
      LSYM=813                                                             10715
      KCFOTB(16)="100000                                                   10720
C<UT>>>>>>>>>>>>>UT>>>>>>>>>>>>>>>>UT>>>>>>>>>>

	CALL HELP
	WRITE(7,2)
2	FORMAT('1ENTER INPUT FILE.EXT',$)
	CALL ASSIGN(LUSI,'AAA:BBBBBB.DAT',-21)
	WRITE(7,4)
4	FORMAT(' ENTER LIST FILE.EXT ',$)
	CALL ASSIGN(LULT,'BBB:ABCDEF.XXX/C',-21)
	WRITE(7,6)
6	FORMAT(' ENTER BINARY FILE.EXT',$)
	CALL ASSIGN(LUOT,'CCC:ABCDEF.XXX',-21)
C<<<UT<<<<<<UT<<<<<<UT<<<<<<<<<<<<<<<
      CALL MPAM0                                                           10725
      STOP                                                                 10730
      END                                                                  10735
      BLOCK DATA                                                           10740
C     NAM: MPADBK  VER: 1.0  DAT: 09-03-75  CMP: PDP-11                    10745
C     PGM: LABELED COMMON FOR 'M68SAM'                                     10750
C                                                                          10755
C     SYS: 'M68SAM'                                                        10760
      COMMON /A/ NOPCD(246),IOPBC2(51),IOPBC3(16)                          10765
      COMMON /A/ IOPBC4(116),IOPBC5(22)                                    10770
      DATA NOPCD(1)/10801/,NOPCD(2)/23130/                                 10775
      DATA NOPCD(3)/23072/,NOPCD(4)/20033/,NOPCD(5)/19744/                 10780
      DATA NOPCD(6)/17742/,NOPCD(7)/17440/,NOPCD(8)/19791/                 10785
      DATA NOPCD(9)/20000/,NOPCD(10)/20545/,NOPCD(11)/18245/               10790
      DATA NOPCD(12)/20306/,NOPCD(13)/18208/,NOPCD(14)/17745/              10795
      DATA NOPCD(15)/21792/,NOPCD(16)/17987/,NOPCD(17)/16928/              10800
      DATA NOPCD(18)/17987/,NOPCD(19)/17184/,NOPCD(20)/17988/              10805
      DATA NOPCD(21)/16928/,NOPCD(22)/21069/,NOPCD(23)/16928/              10810
      DATA NOPCD(24)/21328/,NOPCD(25)/17184/,NOPCD(26)/20304/              10815
      DATA NOPCD(27)/21536/,NOPCD(28)/10802/,NOPCD(29)/20047/              10820
      DATA NOPCD(30)/20512/,NOPCD(31)/21569/,NOPCD(32)/20512/              10825
      DATA NOPCD(33)/21584/,NOPCD(34)/16672/,NOPCD(35)/18766/              10830
      DATA NOPCD(36)/22560/,NOPCD(37)/17477/,NOPCD(38)/22560/              10835
      DATA NOPCD(39)/17228/,NOPCD(40)/22048/,NOPCD(41)/21317/              10840
      DATA NOPCD(42)/22048/,NOPCD(43)/17228/,NOPCD(44)/17184/              10845
      DATA NOPCD(45)/21317/,NOPCD(46)/17184/,NOPCD(47)/17228/              10850
      DATA NOPCD(48)/18720/,NOPCD(49)/21317/,NOPCD(50)/18720/              10855
      DATA NOPCD(51)/21314/,NOPCD(52)/16672/,NOPCD(53)/17218/              10860
      DATA NOPCD(54)/16672/,NOPCD(55)/21569/,NOPCD(56)/16928/              10865
      DATA NOPCD(57)/21570/,NOPCD(58)/16672/,NOPCD(59)/17473/              10870
      DATA NOPCD(60)/16672/,NOPCD(61)/16706/,NOPCD(62)/16672/              10875
      DATA NOPCD(63)/21587/,NOPCD(64)/22560/,NOPCD(65)/18766/              10880
      DATA NOPCD(66)/21280/,NOPCD(67)/20565/,NOPCD(68)/19521/              10885
      DATA NOPCD(69)/20565/,NOPCD(70)/19522/,NOPCD(71)/17477/              10890
      DATA NOPCD(72)/21280/,NOPCD(73)/21592/,NOPCD(74)/21280/              10895
      DATA NOPCD(75)/20563/,NOPCD(76)/18497/,NOPCD(77)/20563/              10900
      DATA NOPCD(78)/18498/,NOPCD(79)/21076/,NOPCD(80)/21280/              10905
      DATA NOPCD(81)/21076/,NOPCD(82)/18720/,NOPCD(83)/22337/              10910
      DATA NOPCD(84)/18720/,NOPCD(85)/21335/,NOPCD(86)/18720/              10915
      DATA NOPCD(87)/20037/,NOPCD(88)/18241/,NOPCD(89)/17231/              10920
      DATA NOPCD(90)/19777/,NOPCD(91)/19539/,NOPCD(92)/21057/              10925
      DATA NOPCD(93)/21071/,NOPCD(94)/21057/,NOPCD(95)/16723/              10930
      DATA NOPCD(96)/21057/,NOPCD(97)/16723/,NOPCD(98)/19521/              10935
      DATA NOPCD(99)/21071/,NOPCD(100)/19521/,NOPCD(101)/17477/            10940
      DATA NOPCD(102)/17217/,NOPCD(103)/18766/                             10945
      DATA NOPCD(104)/17217/,NOPCD(105)/21587/                             10950
      DATA NOPCD(106)/21569/,NOPCD(107)/17228/                             10955
      DATA NOPCD(108)/21057/,NOPCD(109)/20037/                             10960
      DATA NOPCD(110)/18242/,NOPCD(111)/17231/                             10965
      DATA NOPCD(112)/19778/,NOPCD(113)/19539/                             10970
      DATA NOPCD(114)/21058/,NOPCD(115)/21071/                             10975
      DATA NOPCD(116)/21058/,NOPCD(117)/16723/                             10980
      DATA NOPCD(118)/21058/,NOPCD(119)/16723/                             10985
      DATA NOPCD(120)/19522/,NOPCD(121)/21071/                             10990
      DATA NOPCD(122)/19522/,NOPCD(123)/17477/                             10995
      DATA NOPCD(124)/17218/,NOPCD(125)/18766/                             11000
      DATA NOPCD(126)/17218/,NOPCD(127)/21587/                             11005
      DATA NOPCD(128)/21570/,NOPCD(129)/17228/                             11010
      DATA NOPCD(130)/21058/,NOPCD(131)/10803/                             11015
      DATA NOPCD(132)/16979/,NOPCD(133)/21024/                             11020
      DATA NOPCD(134)/16978/,NOPCD(135)/16672/                             11025
      DATA NOPCD(136)/16968/,NOPCD(137)/18720/                             11030
      DATA NOPCD(138)/16972/,NOPCD(139)/21280/                             11035
      DATA NOPCD(140)/16963/,NOPCD(141)/17184/                             11040
      DATA NOPCD(142)/16963/,NOPCD(143)/21280/                             11045
      DATA NOPCD(144)/16974/,NOPCD(145)/17696/                             11050
      DATA NOPCD(146)/16965/,NOPCD(147)/20768/                             11055
      DATA NOPCD(148)/16982/,NOPCD(149)/17184/                             11060
      DATA NOPCD(150)/16982/,NOPCD(151)/21280/                             11065
      DATA NOPCD(152)/16976/,NOPCD(153)/19488/                             11070
      DATA NOPCD(154)/16973/,NOPCD(155)/18720/                             11075
      DATA NOPCD(156)/16967/,NOPCD(157)/17696/                             11080
      DATA NOPCD(158)/16972/,NOPCD(159)/21536/                             11085
      DATA NOPCD(160)/16967/,NOPCD(161)/21536/                             11090
      DATA NOPCD(162)/16972/,NOPCD(163)/17696/                             11095
      DATA NOPCD(164)/10804/,NOPCD(165)/19524/                             11100
      DATA NOPCD(166)/21280/,NOPCD(167)/19524/                             11105
      DATA NOPCD(168)/22560/,NOPCD(169)/17232/                             11110
      DATA NOPCD(170)/22560/,NOPCD(171)/21332/                             11115
      DATA NOPCD(172)/21280/,NOPCD(173)/21332/                             11120
      DATA NOPCD(174)/22560/,NOPCD(175)/19021/                             11125
      DATA NOPCD(176)/20512/,NOPCD(177)/19027/                             11130
      DATA NOPCD(178)/21024/,NOPCD(179)/21333/                             11135
      DATA NOPCD(180)/16961/,NOPCD(181)/17229/                             11140
      DATA NOPCD(182)/20545/,NOPCD(183)/21314/                             11145
      DATA NOPCD(184)/17217/,NOPCD(185)/16718/                             11150
      DATA NOPCD(186)/17473/,NOPCD(187)/16969/                             11155
      DATA NOPCD(188)/21569/,NOPCD(189)/19524/                             11160
      DATA NOPCD(190)/16705/,NOPCD(191)/21332/                             11165
      DATA NOPCD(192)/16705/,NOPCD(193)/17743/                             11170
      DATA NOPCD(194)/21057/,NOPCD(195)/16708/                             11175
      DATA NOPCD(196)/17217/,NOPCD(197)/20306/                             11180
      DATA NOPCD(198)/16705/,NOPCD(199)/16708/                             11185
      DATA NOPCD(200)/17473/,NOPCD(201)/21333/                             11190
      DATA NOPCD(202)/16962/,NOPCD(203)/17229/                             11195
      DATA NOPCD(204)/20546/,NOPCD(205)/21314/                             11200
      DATA NOPCD(206)/17218/,NOPCD(207)/16718/                             11205
      DATA NOPCD(208)/17474/,NOPCD(209)/16969/                             11210
      DATA NOPCD(210)/21570/,NOPCD(211)/19524/                             11215
      DATA NOPCD(212)/16706/,NOPCD(213)/21332/                             11220
      DATA NOPCD(214)/16706/,NOPCD(215)/17743/                             11225
      DATA NOPCD(216)/21058/,NOPCD(217)/16708/                             11230
      DATA NOPCD(218)/17218/,NOPCD(219)/20306/                             11235
      DATA NOPCD(220)/16706/,NOPCD(221)/16708/                             11240
      DATA NOPCD(222)/17474/,NOPCD(223)/10805/                             11245
      DATA NOPCD(224)/20037/,NOPCD(225)/18208/                             11250
      DATA NOPCD(226)/17231/,NOPCD(227)/19744/                             11255
      DATA NOPCD(228)/19539/,NOPCD(229)/21024/                             11260
      DATA NOPCD(230)/21071/,NOPCD(231)/21024/                             11265
      DATA NOPCD(232)/16723/,NOPCD(233)/21024/                             11270
      DATA NOPCD(234)/16723/,NOPCD(235)/19488/                             11275
      DATA NOPCD(236)/21071/,NOPCD(237)/19488/                             11280
      DATA NOPCD(238)/17477/,NOPCD(239)/17184/                             11285
      DATA NOPCD(240)/18766/,NOPCD(241)/17184/                             11290
      DATA NOPCD(242)/21587/,NOPCD(243)/21536/                             11295
      DATA NOPCD(244)/17228/,NOPCD(245)/21024/                             11300
      DATA NOPCD(246)/10809/,IOPBC2(1)/1/,IOPBC2(2)/6/                     11305
      DATA IOPBC2(3)/7/,IOPBC2(4)/8/,IOPBC2(5)/9/,IOPBC2(6)/10/            11310
      DATA IOPBC2(7)/11/,IOPBC2(8)/12/,IOPBC2(9)/13/                       11315
      DATA IOPBC2(10)/14/,IOPBC2(11)/15/,IOPBC2(12)/16/                    11320
      DATA IOPBC2(13)/17/,IOPBC2(14)/22/,IOPBC2(15)/23/                    11325
      DATA IOPBC2(16)/25/,IOPBC2(17)/27/,IOPBC2(18)/48/                    11330
      DATA IOPBC2(19)/49/,IOPBC2(20)/50/,IOPBC2(21)/51/                    11335
      DATA IOPBC2(22)/52/,IOPBC2(23)/53/,IOPBC2(24)/54/                    11340
      DATA IOPBC2(25)/55/,IOPBC2(26)/57/,IOPBC2(27)/59/                    11345
      DATA IOPBC2(28)/62/,IOPBC2(29)/63/,IOPBC2(30)/64/                    11350
      DATA IOPBC2(31)/67/,IOPBC2(32)/68/,IOPBC2(33)/70/                    11355
      DATA IOPBC2(34)/71/,IOPBC2(35)/72/,IOPBC2(36)/73/                    11360
      DATA IOPBC2(37)/74/,IOPBC2(38)/76/,IOPBC2(39)/77/                    11365
      DATA IOPBC2(40)/79/,IOPBC2(41)/80/,IOPBC2(42)/83/                    11370
      DATA IOPBC2(43)/84/,IOPBC2(44)/86/,IOPBC2(45)/87/                    11375
      DATA IOPBC2(46)/88/,IOPBC2(47)/89/,IOPBC2(48)/90/                    11380
      DATA IOPBC2(49)/92/,IOPBC2(50)/93/,IOPBC2(51)/95/                    11385
      DATA IOPBC3(1)/141/,IOPBC3(2)/32/,IOPBC3(3)/34/                      11390
      DATA IOPBC3(4)/35/,IOPBC3(5)/36/,IOPBC3(6)/37/                       11395
      DATA IOPBC3(7)/38/,IOPBC3(8)/39/,IOPBC3(9)/40/                       11400
      DATA IOPBC3(10)/41/,IOPBC3(11)/42/,IOPBC3(12)/43/                    11405
      DATA IOPBC3(13)/44/,IOPBC3(14)/45/,IOPBC3(15)/46/                    11410
      DATA IOPBC3(16)/47/,IOPBC4(1)/158/,IOPBC4(2)/174/                    11415
      DATA IOPBC4(3)/142/,IOPBC4(4)/190/,IOPBC4(5)/222/                    11420
      DATA IOPBC4(6)/238/,IOPBC4(7)/206/,IOPBC4(8)/254/                    11425
      DATA IOPBC4(9)/156/,IOPBC4(10)/172/,IOPBC4(11)/140/                  11430
      DATA IOPBC4(12)/188/,IOPBC4(13)/159/,IOPBC4(14)/175/                 11435
      DATA IOPBC4(15)/1/,IOPBC4(16)/191/,IOPBC4(17)/223/                   11440
      DATA IOPBC4(18)/239/,IOPBC4(19)/1/,IOPBC4(20)/255/                   11445
      DATA IOPBC4(21)/126/,IOPBC4(22)/110/,IOPBC4(23)/1/                   11450
      DATA IOPBC4(24)/126/,IOPBC4(25)/189/,IOPBC4(26)/173/                 11455
      DATA IOPBC4(27)/1/,IOPBC4(28)/189/,IOPBC4(29)/144/                   11460
      DATA IOPBC4(30)/160/,IOPBC4(31)/128/,IOPBC4(32)/176/                 11465
      DATA IOPBC4(33)/145/,IOPBC4(34)/161/,IOPBC4(35)/129/                 11470
      DATA IOPBC4(36)/177/,IOPBC4(37)/146/,IOPBC4(38)/162/                 11475
      DATA IOPBC4(39)/130/,IOPBC4(40)/178/,IOPBC4(41)/148/                 11480
      DATA IOPBC4(42)/164/,IOPBC4(43)/132/,IOPBC4(44)/180/                 11485
      DATA IOPBC4(45)/149/,IOPBC4(46)/165/,IOPBC4(47)/133/                 11490
      DATA IOPBC4(48)/181/,IOPBC4(49)/150/,IOPBC4(50)/166/                 11495
      DATA IOPBC4(51)/134/,IOPBC4(52)/182/,IOPBC4(53)/151/                 11500
      DATA IOPBC4(54)/167/,IOPBC4(55)/1/,IOPBC4(56)/183/                   11505
      DATA IOPBC4(57)/152/,IOPBC4(58)/168/,IOPBC4(59)/136/                 11510
      DATA IOPBC4(60)/184/,IOPBC4(61)/153/,IOPBC4(62)/169/                 11515
      DATA IOPBC4(63)/137/,IOPBC4(64)/185/,IOPBC4(65)/154/                 11520
      DATA IOPBC4(66)/170/,IOPBC4(67)/138/,IOPBC4(68)/186/                 11525
      DATA IOPBC4(69)/155/,IOPBC4(70)/171/,IOPBC4(71)/139/                 11530
      DATA IOPBC4(72)/187/,IOPBC4(73)/208/,IOPBC4(74)/224/                 11535
      DATA IOPBC4(75)/192/,IOPBC4(76)/240/,IOPBC4(77)/209/                 11540
      DATA IOPBC4(78)/225/,IOPBC4(79)/193/,IOPBC4(80)/241/                 11545
      DATA IOPBC4(81)/210/,IOPBC4(82)/226/,IOPBC4(83)/194/                 11550
      DATA IOPBC4(84)/242/,IOPBC4(85)/212/,IOPBC4(86)/228/                 11555
      DATA IOPBC4(87)/196/,IOPBC4(88)/244/,IOPBC4(89)/213/                 11560
      DATA IOPBC4(90)/229/,IOPBC4(91)/197/,IOPBC4(92)/245/                 11565
      DATA IOPBC4(93)/214/,IOPBC4(94)/230/,IOPBC4(95)/198/                 11570
      DATA IOPBC4(96)/246/,IOPBC4(97)/215/,IOPBC4(98)/231/                 11575
      DATA IOPBC4(99)/1/,IOPBC4(100)/247/,IOPBC4(101)/216/                 11580
      DATA IOPBC4(102)/232/,IOPBC4(103)/200/,IOPBC4(104)/248/              11585
      DATA IOPBC4(105)/217/,IOPBC4(106)/233/,IOPBC4(107)/201/              11590
      DATA IOPBC4(108)/249/,IOPBC4(109)/218/,IOPBC4(110)/234/              11595
      DATA IOPBC4(111)/202/,IOPBC4(112)/250/,IOPBC4(113)/219/              11600
      DATA IOPBC4(114)/235/,IOPBC4(115)/203/,IOPBC4(116)/251/              11605
      DATA IOPBC5(1)/96/,IOPBC5(2)/112/,IOPBC5(3)/99/                      11610
      DATA IOPBC5(4)/115/,IOPBC5(5)/100/,IOPBC5(6)/116/                    11615
      DATA IOPBC5(7)/102/,IOPBC5(8)/118/,IOPBC5(9)/103/                    11620
      DATA IOPBC5(10)/119/,IOPBC5(11)/104/,IOPBC5(12)/120/                 11625
      DATA IOPBC5(13)/105/,IOPBC5(14)/121/,IOPBC5(15)/106/                 11630
      DATA IOPBC5(16)/122/,IOPBC5(17)/108/,IOPBC5(18)/124/                 11635
      DATA IOPBC5(19)/109/,IOPBC5(20)/125/,IOPBC5(21)/111/                 11640
      DATA IOPBC5(22)/127/                                                 11645
      END                                                                  11650
      SUBROUTINE MPAM0                                                     11655
C+    NAM: MPAM0   VER: 1.0  DAT: 09-03-75  CMP: ALL                       11660
C     PGM: MAIN ROUTINE FOR THE CROSS-ASSEMBLER                            11665
C                                                                          11670
C     SYS: M68SAM                                                          11675
C                                                                          11680
C     ENT: N/A                                                             11685
C     RTN: N/A                                                             11690
C                                                                          11695
C     FNC: MAIN CONTROL ROUTINE FOR THE CROSS-ASSEMBLER.                   11700
C*                                                                         11705
      COMMON ICOMON,LULT,LUSI(2),LLSPSP,IBPWD(231),NPNAM(4)                11710
      COMMON NPGNO(140),IPASS,IOPCLS(16),NOPT(2),NOPC(5),IXBASE            11715
      EQUIVALENCE (NOPBAS,NOPT(1)),(NOPLI,NOPT(2))                         11720
C***  INITIALIZE THE ASSEMBLER                                             11725
C                                                                          11730
C***  SPACE THE PAGE HEADER'S PROGRAM NAME                                 11735
      DO 100I=1,4                                                          11740
      NPNAM(I)=LLSPSP                                                      11745
100   CONTINUE                                                             11750
      CALL MPAPAG (0)                                                      11755
C***  PRINT PROPIETARY MESSAGE                                             11760
      WRITE (LULT,110)                                                     11765
110   FORMAT (9X,45H M68SAM IS THE PROPERTY OF MOTOROLA SPD, INC.)         11770
      WRITE (LULT,120)                                                     11775
120   FORMAT(12X,39HCOPYRIGHT 1974 AND 1975 BY MOTOROLA INC)               11780
      WRITE (LULT,140)                                                     11785
      WRITE (LULT,130)                                                     11790
130   FORMAT(10X,43HMOTOROLA M6800 CROSS ASSEMBLER, RELEASE 1.2)           11795
      WRITE (LULT,140)                                                     11800
140   FORMAT (1H )                                                         11805
      CALL MPAPAG (5)                                                      11810
C***  CALL PASS 1 BUILD SYMBOL TABLE                                       11815
      IPASS=1                                                              11820
      CALL MPAM1                                                           11825
C***  INITIALIZE PASS 2                                                    11830
      NOPLI=2                                                              11835
      IXBASE=3                                                             11840
      NOPBAS=16                                                            11845
C***  CALL PASS 2 OUTPUT LISTING AND TAPE FILE                             11850
      IPASS=2                                                              11855
      CALL MPAM2                                                           11860
C***  CALL PASS 3 LISTING SYMBOL TABLE AND CLOSE FILES                     11865
      CALL MPAM3                                                           11870
      RETURN                                                               11875
      END                                                                  11880
      SUBROUTINE MPAPAG (JNL)                                              11885
C+    NAM: MPAPAG  VER: 1.2  DAT: 08-27-75  CMP: ALL                       11890
C                                                                          11895
C     SYS: M68SAM                                                          11900
C                                                                          11905
C     ENT: JNL - 0 GO TO TOP OF PAGE                                       11910
C              - >0 NUMBER OF LINES PRINTED                                11915
C                                                                          11920
C     RTN: JNL - N/C                                                       11925
C                                                                          11930
C     FNC: COUNT EACH LINE PRINTED AND GO TO TOP OF THE                    11935
C          PAGE EACH TIME WHEN THE PAGE OVERFLOWS.                         11940
C                                                                          11945
C     REV: 1.2 - NPNAM CHANGED TO 4 WORDS                                  11950
C*                                                                         11955
      COMMON ICOMON,LULT,LUSI(234),NPNAM(4),NPGNO,IPGLOL,IPGLEN            11960
      COMMON NPLCT,ICSHF(153),NOPT(2)                                      11965
      EQUIVALENCE (NOPLI,NOPT(2))                                          11970
      DATA JPVFCC/1H1/                                                     11975
      IF(NOPLI.EQ.1) RETURN                                                11980
100   IF(NPGNO.EQ.0) GO TO 130                                             11985
      IF(JNL.LT.1) GO TO 110                                               11990
      NPLCT=NPLCT+JNL                                                      11995
      IF(NPLCT.LT.IPGLEN-7) RETURN                                         12000
C***  PAGE OVERFLOW, GO TO TOP OF THE NEXT PAGE                            12005
110   IF(NPLCT.GE.IPGLEN) GO TO 130                                        12010
      NPLCT=NPLCT+1                                                        12015
      WRITE (LULT,120)                                                     12020
120   FORMAT (1H )                                                         12025
      GO TO 110                                                            12030
130   WRITE (LULT,140)                                                     12035
140   FORMAT (4H ---)                                                      12040
      WRITE (LULT,120)                                                     12045
      WRITE (LULT,120)                                                     12050
150   NPGNO=NPGNO+1                                                        12055
      WRITE (LULT,160) JPVFCC,NPNAM,NPGNO                                  12060
160   FORMAT(5A2,7X,30HMOTOROLA M68SAM CROSS-ASSMBLER,9X,4HPAGE,I3)
      DO 170I=1,3                                                          12070
      WRITE (LULT,120)                                                     12075
170   CONTINUE                                                             12080
      NPLCT=4                                                              12085
      RETURN                                                               12090
      END                                                                  12095
      SUBROUTINE MPAPA1 (JPSW,JBUF,JLEN)                                   12100
C+    NAM: MPAPA1  VER: 1.0  DAT: 10-01-74  CMP: ALL                       12105
C     PGM: PRINT BUFFER IN 'A1' FORMAT                                     12110
C                                                                          12115
C     SYS: M68SAM                                                          12120
C                                                                          12125
C     ENT: JPSW - 1=> DONOT PRINT                                          12130
C               - 2=> PRINT                                                12135
C          JBUF - BUFFER TO PRINT                                          12140
C          JLEN - MAX NUMBER OF CHARACTERS IN JBUF                         12145
C                                                                          12150
C     RTN: JPSW - N/C                                                      12155
C          JBUF - BUFFER CONVERTED TO COMPUTER'S A1 FORMAT                 12160
C          JLEN - N/C                                                      12165
C                                                                          12170
C     FNC: CONVERT THE BUFFER FROM SYSTEM'S R1 FORMAT TO                   12175
C          THE COMPUTER'S A1 FORMAT AND THAN PRINT ONLY                    12180
C          THE NON-SPACES (SKIP TRAILING BLANKS).                          12185
C*                                                                         12190
      COMMON ICOMON,LULT,LUSI(2),LLSPSP                                    12195
      INTEGER JBUF(140)                                                    12200
      CALL MPUCA1 (JBUF,JLEN)                                              12205
      L=1                                                                  12210
      DO 10 I=1,JLEN                                                       12215
      IF(JBUF(I).NE.LLSPSP) L=I                                            12220
10    CONTINUE                                                             12225
      WRITE (LULT,11) (JBUF(I),I=1,L)                                      12230
11    FORMAT (140A1)                                                       12235
      RETURN                                                               12240
      END                                                                  12245
      FUNCTION MPUBSM (JWD,JSB,JMK)                                        12250
C+    NAM: MPUBSM  VER: 1.1  DAT: 04-28-75  CMP: ALL                       12255
C     PGM: FUNCTION ROUTINE TO SHIFT AND MASK BITS                         12260
C                                                                          12265
C     SYS: ALL                                                             12270
C                                                                          12275
C                                                                          12280
C     ENT: JWD - WORD TO SHIFT                                             12285
C          JSB - BIT POSITION TO SHIFT TO BIT 1                            12290
C          JMK - BIT MASK AFTER THE SHIFT                                  12295
C                                                                          12300
C     RTN: JWD - N/C                                                       12305
C          JSB - N/C                                                       12310
C          JMK - N/C                                                       12315
C                                                                          12320
C     FNC: SHIFT THE WORD JWD TO THE LEFT 'JSB' POSITIONS                  12325
C          'JSB' IS FROM 1 TO 16. BITS ARE NUMBERED FROM                   12330
C          LEFT TO RIGTH (A 1 => NO SHIFT, 2 => SHIFT THE WORD             12335
C          LEFT 1 BIT). AFTER THE SHIFT THE WORD IS 'ANDED'                12340
C          WITH 'JMK' AND THE RESULT ARE RETURN AS A FUNCTION              12345
C          RETURN (N=MPUBSM(JWD,JSB,JMK)).                                 12350
C                                                                          12355
C     REV: 1.1 - ROUTINE WAS MADE MACHINE INDEPENDENT                      12360
C*                                                                         12365
      J = MPUXBS(JWD, 16, JSB)                                             12370
      MPUBSM=MPUAND(J,JMK)                                                 12375
      RETURN                                                               12380
      END                                                                  12385
      SUBROUTINE MPUCNA(JN,JBAS,JCOL)                                      12390
C+    NAM: MPUCNA  VER: 1.5  DAT: 09-03-75  CMP: ALL                       12395
C     PGM: CONVERT BINARY NUMBER TO ASCII                                  12400
C                                                                          12405
C     SYS: ALL                                                             12410
C                                                                          12415
C     ENT: JN  -BINARY NUMBER TO BE CONVERTED                              12420
C          JBAS-BASE TO USE IN CONVERSION                                  12425
C          JCOL-NUMBER OF COLUMNS OF CONVERTED OUTPUT                      12430
C                                                                          12435
C     RTN: JN  -N/C                                                        12440
C          JBAS-N/C                                                        12445
C          JCOL-N/C                                                        12450
C                                                                          12455
C     FNC: ROUTINE CONVERTS BINARY INTEGER TO ASCII OUTPUT IN              12460
C          THE REQUESTED BASE.  ASCII OUTPUT IS PLACED IN ARRAY            12465
C          "NB" BY ROUTINE MPUSNC.                                         12470
C                                                                          12475
C     REV: 1.1 - ROUTINE WAS MADE MACHINE INDEPENDENT                      12480
C          1.2 - FIX CODE FOR 1.1 CHANGE                                   12485
C          1.3 - IMPLEMENTED MPVMUL/MPVDIV ROUTINES                        12490
C          1.4 - INCREASED SIZE OF ARRAY "ID" TO 7 WORDS                   12495
C          1.5 - FIXED NEG NBR CONVERSION PROB FOR 1'S COMP MACHINES       12500
C*                                                                         12505
      COMMON ICOMON(5),IBPWD,KCFOTB(409),L0                                12510
      INTEGER ID(7)                                                        12515
      NN = JN                                                              12520
      IF (IBPWD .GT. 16) GO TO 20                                          12525
C                                                                          12530
C***  CONVERSION FOR MACHINES WITH WORDSIZE = 16 BITS                      12535
C                                                                          12540
      DO 10 I = 1, JCOL                                                    12545
      CALL MPVDIV(NN, JBAS, J)                                             12550
      CALL MPVMUL(J, JBAS, JTMP)                                           12555
      ID(I) = NN - JTMP                                                    12560
      NN = J                                                               12565
10    CONTINUE                                                             12570
      GO TO 40                                                             12575
C                                                                          12580
C***  CONVERSION FOR MACHINES WITH WORDSIZE > 16 BITS                      12585
C                                                                          12590
20    CALL MPUNEG(NN)                                                      12595
      DO 30 I = 1, JCOL                                                    12600
      J=NN/JBAS                                                            12605
      ID(I)=NN-(J*JBAS)                                                    12610
      NN=J                                                                 12615
30    CONTINUE                                                             12620
40    I = JCOL                                                             12625
50    NN = ID(I)                                                           12630
      IF(NN.GT.9) NN=NN+7                                                  12635
      CALL MPUSNC (NN+L0)                                                  12640
      I=I-1                                                                12645
      IF(I.GT.0) GO TO 50                                                  12650
      RETURN                                                               12655
      END                                                                  12660
      SUBROUTINE MPUSNC (JCHR)                                             12665
C+    NAM: MPUSNC  VER: 1.0  DAT: 12-29-73  CMP: ALL                       12670
C     PGM: P#                                                              12675
C                                                                          12680
C     SYS: ALL                                                             12685
C                                                                          12690
C     ENT: N/A                                                             12695
C                                                                          12700
C     RTN: N/A                                                             12705
C                                                                          12710
C     FNC:                                                                 12715
C                                                                          12720
C     GEN:                                                                 12725
C                                                                          12730
C     REV: N/A                                                             12735
C*                                                                         12740
      COMMON ICOMON(105),NB(121),INX                                       12745
      INX=INX+1                                                            12750
      NB(INX)=JCHR                                                         12755
      RETURN                                                               12760
      END                                                                  12765
      FUNCTION MPUXBS(JWORD, JMSB, JLSB)                                   12770
C+    NAM: MPUXBS  VER: 1.2  DAT: 08-29-75  CMP: ALL                       12775
C     PGM: EXTRACT BIT STRING                                              12780
C                                                                          12785
C     SYS: ALL                                                             12790
C                                                                          12795
C     ENT: JWORD - WORD FROM WHICH TO EXTRACT BIT STRING                   12800
C          JMSB  - BIT NUMBER OF MSB BIT                                   12805
C          JLSB  - BIT NUMBER OF LSB BIT                                   12810
C                                                                          12815
C     RTN: JWORD - N/C                                                     12820
C          JMSB  - N/C                                                     12825
C          JLSB  - N/C                                                     12830
C                                                                          12835
C     FNC: FUNCTION IS SET TO THE VALUE OF THE BIT STRING (RIGHT           12840
C          JUSTIFIED).                                                     12845
C                                                                          12850
C     GEN: BIT 16 IS MSB.  BIT 1 IS LSB.                                   12855
C                                                                          12860
C     REV 1.1 - MPUAND USE INSTEAD OF MPUTBT                               12865
C         1.2 - CALL TO MPUOR CHANGED TO MPUIOR                            12870
C*                                                                         12875
      COMMON ICOMON(6),KCFOTB(16)                                          12880
      JBS = 0                                                              12885
      K = 0                                                                12890
      DO 10 I = JLSB, JMSB                                                 12895
      K = K + 1                                                            12900
      IF(MPUAND(JWORD,KCFOTB(I)).NE.0) JBS=MPUIOR(JBS,KCFOTB(K))           12905
10    CONTINUE                                                             12910
      MPUXBS = JBS                                                         12915
      RETURN                                                               12920
      END                                                                  12925
      FUNCTION MPUAND (JA,JB)                                              12930
C+    NAM: MPUAND  VER: 1.0  DAT: 08-28-74  CMP: PDP-11                    12935
C     PGM: FUNCTION ROUTINE TO 'ANDED'                                     12940
C                                                                          12945
C     SYS: ALL                                                             12950
C                                                                          12955
C     ENT: JA - 1ST VALUE TO BE 'ANDED'                                    12960
C          JB - 2ND VALUE TO BE 'ANDED'                                    12965
C                                                                          12970
C     RTN: JA - N/C                                                        12975
C          JB - N/C                                                        12980
C                                                                          12985
C     FNC: THE FUNCTION IS SET TO LOGICAL PRODUCT OF JA AND JB             12990
C                                                                          12995
C     GEN:                                                                 13000
C                                                                          13005
C     REV: N/A                                                             13010
C*                                                                         13015
      MPUAND = JA .AND. JB                                                 13020
      RETURN                                                               13025
      END                                                                  13030
      SUBROUTINE MPUCA1(JLST, JN)                                          13035
C+    NAM: MPUCA1  VER: 1.0  DAT: 08-28-74  CMP: PDP-11                    13040
C     PGM: CONVERT TO A1 FORMAT                                            13045
C                                                                          13050
C     SYS: ALL                                                             13055
C                                                                          13060
C     ENT: JLST - ARRAY CONTAINING THE CHARACTERS, 1 PER WORD              13065
C                 RIGHT JUSTIFIED.                                         13070
C          JN   - NUMBER OF WORDS IN ARRAY                                 13075
C                                                                          13080
C     RTN: JLST - ARRAY CONTAINING CHARACTERS, 1 PER WORD                  13085
C                 IN COMPUTER'S A1 FORMAT.                                 13090
C          JN   - N/C                                                      13095
C                                                                          13100
C     FNC: CONVERT THE CHARACTERS IN ARRAY 'JLST' TO COMPUTER'S            13105
C          A1 FORMAT.                                                      13110
C                                                                          13115
C     GEN:                                                                 13120
C                                                                          13125
C     REV: N/A                                                             13130
C*                                                                         13135
      INTEGER JLST(80)                                                     13140
      DO 10 I = 1, JN                                                      13145
      JLST(I) = JLST(I) + "20000                                           13150
10    CONTINUE                                                             13155
      RETURN                                                               13160
      END                                                                  13165
      FUNCTION MPUIOR (JA,JB)                                              13170
C+    NAM: MPUIOR  VER: 1.0  DAT: 09-02-75  CMP: PDP-11                    13175
C     PGM: LOGICAL "OR" 'JA' AND 'JB'                                      13180
C                                                                          13185
C     SYS: ALL                                                             13190
C                                                                          13195
C     ENT: JA - 1ST VALUE TO BE "ORED"                                     13200
C          JB - 2ND VALUE TO BE "ORED"                                     13205
C                                                                          13210
C     RTN: JA - N/C                                                        13215
C          JB - N/C                                                        13220
C                                                                          13225
C     FNC: THE  FUNCTION IS SET TO THE LOGICAL SUM OF JA AND JB            13230
C                                                                          13235
C     REV: N/A                                                             13240
C*                                                                         13245
      MPUIOR = JA .OR. JB                                                  13250
      RETURN                                                               13255
      END                                                                  13260
      SUBROUTINE MPUNEG(JNBR)                                              13265
C+    NAM: MPUNEG  VER: 1.0  DAT: 08-28-74  CMP: PDP-11                    13270
C     PGM: 2'S COMPLEMENT, 16 BIT NEGATE ROUTINE                           13275
C                                                                          13280
C     SYS: ALL                                                             13285
C                                                                          13290
C     ENT: JNBR - NUMBER TO BE CONVERTED TO COMPLEMENT FORM                13295
C                                                                          13300
C     RTN: JNBR - CONVERTED FORM IF IT WAS NEG AT ENTRY                    13305
C                                                                          13310
C     FNC: JNBR IS SET TO THE 2'S COMPLEMENT, 16 BIT FORM OF               13315
C          ITS VALUE IF IT IS NEGATIVE ON ENTRY.                           13320
C                                                                          13325
C     GEN:                                                                 13330
C                                                                          13335
C     REV: N/A                                                             13340
C*                                                                         13345
      RETURN                                                               13350
      END                                                                  13355
      SUBROUTINE MPARSI (JSW)                                              13360
C+    NAM: MPARSI  VER: 1.2  DAT: 09-03-75  CMP: PDP-11                    13365
C     PGM: OPEN, READ, REWIND AND READ SI FILE                             13370
C                                                                          13375
C     SYS: M68SAM                                                          13380
C                                                                          13385
C     ENT: JSW - 1=> OPEN SI FILE                                          13390
C              - 2=> READ SI PASS 1                                        13395
C              - 3=> REWIND FOR PASS 2                                     13400
C              - 4=> READ SI PASS 2                                        13405
C                                                                          13410
C     RTN: JSW - N/C                                                       13415
C                                                                          13420
C     FNC: THIS ROUTINE IS 1ST CALLED TO OPEN THE SI FILE                  13425
C          THEN IT IS CALLED TO READ EACH RECORD FOR PASS 1                13430
C          WHEN A 'END' RECORD IS READ IT IS CALLED TO REWIND              13435
C          THE FILE AND RE-READ EACH RECORD FOR PASS 2.                    13440
C                                                                          13445
C     REV: N/A                                                             13450
C*                                                                         13455
      COMMON ICOMON(2),LUSI,LUOT(22),ISIBUF(80)                            13460
      GO TO (100,200,400,300),JSW                                          13465
C***  OPEN IT                                                              13470
100   RETURN                                                               13475
C***  READ PASS 1                                                          13480
200   CONTINUE                                                             13485
C***  READ PASS 2                                                          13490
300   READ (LUSI,310) ISIBUF                                               13495
310   FORMAT (80A1)                                                        13500
      RETURN                                                               13505
C***  REWIND SI                                                            13510
400   REWIND LUSI                                                          13515
      RETURN                                                               13520
      END                                                                  13525
      SUBROUTINE MPAM1                                                     13530
C+    NAM: MPAM1   VER: 1.2  DAT: 09-03-75  CMP: ALL                       13535
C     PGM: PASS ONE OF THE M6800 CROSS ASSEMBLER                           13540
C                                                                          13545
C     SYS: M68SAM                                                          13550
C                                                                          13555
C     ENT: N/A                                                             13560
C     RTN: N/A                                                             13565
C                                                                          13570
C     FNC: PASS ONE, BUILD THE SYMBOL TABLE                                13575
C                                                                          13580
C     REV: 1.2 - CALL TO MPARSI ADDED                                      13585
C*                                                                         13590
      COMMON ICOMON(2),LUSI,LUOT,LLSPSP,IBPWD(20),ISIBUF(80)               13595
      COMMON NB(122),LABEL(4),LABESW,IOPCOD(4),NPNAM(4)                    13600
      COMMON NPGNO(94),ISIMBF(43),IC,NC,ICSW,IPASS,IOPCLS                  13605
      COMMON IOPIXB,IOPBIN(2),IPCT,ITERR,NRTSW(3),NAMSW,LSCM               13610
      COMMON LIMA(13),LSPSP,L8SP(3),LSP,LASK(8),LA,LB                      13615
      EQUIVALENCE (ISILN,ISIMBF(2))                                        13620
      COMMON /A/ NOPCD(246),IOPBC2(51),IOPBC3(16)                          13625
      COMMON /A/ IOPBC4(116),IOPBC5(22)                                    13630
      IE=204                                                               13635
      JRDNO=0                                                              13640
      JRLNSW=2                                                             13645
C***  OPEN THE SOURCE INPUT FILE                                           13650
      CALL MPARSI (1)                                                      13655
C***  SETUP THE P.T. FILE                                                  13660
      CALL MPUPTS (1,0,0)                                                  13665
      GO TO 140                                                            13670
C***  ERROR '206' SYMBOL HAS BEEN DEFINED, (DOUBLE DEFINDED)               13675
100   IF(LABESW.EQ.2) CALL MPAERR (206)                                    13680
      LABESW=1                                                             13685
      GO TO (110,120),NAMSW                                                13690
C***  ERROR '201' NO NAM RECORD OR MULT NAM RECORDS                        13695
110   CALL MPAERR (201)                                                    13700
      NAMSW=2                                                              13705
120   IF(ITERR.NE.JTERR) CALL MPAPRL (545)                                 13710
140   JTERR=ITERR                                                          13715
      DO 150 I=1,80                                                        13720
      ISIBUF(I)=LLSPSP                                                     13725
150   CONTINUE                                                             13730
C***  READ IN ONE SOURCE STATEMENT                                         13735
      CALL MPARSI (2)                                                      13740
      JRDNO=JRDNO+1                                                        13745
      ISILN=JRDNO                                                          13750
C***  PACK THE SOURCE RECORD INTO THE SOURCE IMAGE BUFFER                  13755
      CALL MPUPIB (ISIBUF,JRLNSW)                                          13760
      LEVEL=1                                                              13765
      LABEL(1)=0                                                           13770
      LABEL(4)=0                                                           13775
      GO TO 180                                                            13780
C***  GET THE NEXT CHAR                                                    13785
170   CALL MPUGNC (NC)                                                     13790
180   GO TO (190,260,280,310),LEVEL                                        13795
190   GO TO (250,220,200,240,230,210),ICSW                                 13800
C***  ERROR '202' LABEL OR OPCODE MUST START WITH A ALPHA CHAR             13805
200   IE=IE-1                                                              13810
C***  ERROR '203' BLANK RECORD OR THE RECORD ONLY CONTAINS A LABEL         13815
210   IE=IE-1                                                              13820
C***  ERROR '204' SYNTAX ERROR                                             13825
220   CALL MPAERR (IE)                                                     13830
      IE=204                                                               13835
      GO TO 440                                                            13840
230   IF (NC.NE.LSCM) GO TO 220                                            13845
C***  RECORD IS A COMMENT RECORD (SKIPIT)                                  13850
      GO TO 100                                                            13855
C***  GET THE STATEMENT'S LABEL                                            13860
240   CALL MPUBN8 (LABEL)                                                  13865
      LABEL(4)=0                                                           13870
C***  ERROR '205' ILLEGAL STATEMENT LABEL (MUST END WITH A SPACE)          13875
      IF (ICSW.NE.1) CALL MPAERR (205)                                     13880
C***  STORE THE LABEL IN THE SYMBOL TABLE                                  13885
      LABEL(4)=IPCT                                                        13890
      CALL MPASSY (LABEL,1,LABESW)                                         13895
250   LEVEL=2                                                              13900
C***  LEVEL 2 SCAN FOR START OF OPCODE FIELD                               13905
260   GO TO (170,200,200,270,200,210),ICSW                                 13910
C***  GET THE OPCODE                                                       13915
270   CALL MPUBN8 (IOPCOD)                                                 13920
      IF(ICSW.NE.1) GO TO 220                                              13925
      CALL MPAFOP                                                          13930
      IF(IOPCLS.EQ.2) GO TO 460                                            13935
      LEVEL=3                                                              13940
C***  LEVEL 3 SCAN FOR 'A ' OR 'B '                                        13945
280   GO TO (170,320,320,290,320,320),ICSW                                 13950
290   IF(NC.NE.LA.AND.NC.NE.LB) GO TO 320                                  13955
      JC=NC                                                                13960
      CALL MPUGNC(NC)                                                      13965
      IF(ICSW.EQ.1) GO TO 300                                              13970
      IC=IC-2                                                              13975
      CALL MPUGNC(NC)                                                      13980
      GO TO 320                                                            13985
C***  OPCODE IS 'CCC X ' WHERE) X= A OR B                                  13990
300   IOPCOD(2)=IOPCOD(2)-LSP+JC                                           13995
      CALL MPAFOP                                                          14000
      IF(IOPCLS.EQ.2) GO TO 460                                            14005
C***  SCAN TO THE START OF THE OPERAND FIELD                               14010
      LEVEL=4                                                              14015
310   GO TO (170,320,320,320,320,320),ICSW                                 14020
320   GO TO (330,460,450,420,400),IOPCLS                                   14025
C                                                                          14030
C***  DIRECTIVE OPCODES                                                    14035
C                                                                          14040
C***  ASSEMBLER CLASS 1                                                    14045
330   IF(IOPIXB.GT.4) GO TO 390                                            14050
      GO TO (340,350,360,340),IOPIXB                                       14055
C***  ERROR '207' UNDEFINED OPCODE                                         14060
340   CALL MPAERR (207)                                                    14065
      GO TO 440                                                            14070
C***  'NAM' OPCODE                                                         14075
350   IF(NAMSW.EQ.2) GO TO 110                                             14080
      CALL MPUBN8 (NPNAM)                                                  14085
      NAMSW=2                                                              14090
      IF(NPNAM(1).EQ.LSPSP) GO TO 220                                      14095
      CALL MPUCA2(NPNAM,4)                                                 14100
      GO TO 100                                                            14105
C***  'END' OPCODE                                                         14110
360   RETURN                                                               14115
C***  GO TO THE DIRECTIVE OPCODE PROCESSOR                                 14120
390   CALL MPAPSC                                                          14125
      IF(IPCT.EQ.0) GO TO 120                                              14130
      GO TO 100                                                            14135
C                                                                          14140
C***  2 OR 3 BYTE INSTRUCTION (INDEXED AND EXTENDED MODE ONLY)             14145
C                                                                          14150
C***  ASSEMBLER CLASS 5                                                    14155
400   CALL MPAGAM (IAMOD,JOPRSW)                                           14160
      IF(JOPRSW.GT.4) GO TO 220                                            14165
      GO TO (440,450,410,440),IAMOD                                        14170
410   CALL MPAERR (209)                                                    14175
      GO TO 440                                                            14180
C                                                                          14185
C***  2 OR 3 BYTE INSTRUCTION                                              14190
C                                                                          14195
C***  ASSEMBLER CLASS 4                                                    14200
420   CALL MPAGAM (IAMOD,JOPRSW)                                           14205
      IF(JOPRSW.GT.4) GO TO 220                                            14210
      IOPIXB=IOPIXB*4-4+IAMOD                                              14215
      IF(IOPBC4(IOPIXB).LT.0) GO TO 410                                    14220
      IF(IOPIXB.LT.13.AND.IAMOD.EQ.3) GO TO 440                            14225
      GO TO (430,450,450,440),IAMOD                                        14230
430   IF(IOPIXB.NE.21.AND.IOPIXB.NE.25) GO TO 450                          14235
C***  3 WORD OPCODES                                                       14240
440   IPCT=IPCT+1                                                          14245
C***  2 WORD OPCODES                                                       14250
450   IPCT=IPCT+1                                                          14255
C***  1 WORD OPCODES                                                       14260
460   IPCT=IPCT+1                                                          14265
      GO TO 100                                                            14270
      END                                                                  14275
      SUBROUTINE MPAERR (JER)                                              14280
C+    NAM: MPAERR  VER: 1.0  DAT: 10-01-74  CMP: ALL                       14285
C     PGM: CROSS-ASSEMBLER ERROR ROUTINE                                   14290
C                                                                          14295
C     SYS: M68SAM                                                          14300
C                                                                          14305
C     ENT: JER - ERROR NUMBER TO PRINT                                     14310
C                                                                          14315
C     RTN: JER - N/C                                                       14320
C                                                                          14325
C     FNC: PRINT THE ERROR MESSAGE IF THE OPTIONS ARE SET.                 14330
C          SHORT ERRORS PRINT ONLY THE ERROR CODE LONG                     14335
C          ERROR PRINT THE LAST 6 CHAR WHERE THE SCAN HAS                  14340
C          STOP AT.                                                        14345
C*                                                                         14350
      COMMON ICOMON,LULT,LUSI(242),ICSHF,IOTBUF(132),IC,NC(8)              14355
      COMMON ITERR,NRTSW(18),LSPSP,L8SP(3),LSP                             14360
      INTEGER JB(3)                                                        14365
C***  PRINT THE ERROR MESSAGE                                              14370
      DO 100 I=1,3                                                         14375
      JB(I)=LSPSP                                                          14380
100   CONTINUE                                                             14385
C***  PRINT LONG ERROR MESSAGES                                            14390
      JSIC=IC                                                              14395
      IC=JSIC-6                                                            14400
      IF(IC.LT.0) IC=0                                                     14405
      I=1                                                                  14410
200   CALL MPUGNC (J)                                                      14415
      JB(I)=J*ICSHF+LSP                                                    14420
      IF(IC.EQ.JSIC) GO TO 300                                             14425
      CALL MPUGNC (J)                                                      14430
      JB(I)=JB(I)-LSP+J                                                    14435
      I=I+1                                                                14440
      IF(IC.LT.JSIC) GO TO 200                                             14445
C***  PRINT THE ERROR MESSAGE                                              14450
300   CALL MPUCA2 (JB,3)                                                   14455
      WRITE (LULT,301) JER,JB                                              14460
301   FORMAT(10H ****ERROR,I4,2X,3A2)                                      14465
      CALL MPAPAG (1)                                                      14470
      ITERR=ITERR+1                                                        14475
      RETURN                                                               14480
      END                                                                  14485
      SUBROUTINE MPAFOP                                                    14490
C+    NAM: MPAFOP  VER: 1.0  DAT: 10-01-74  CMP: ALL                       14495
C     PGM: SEARCH THE OPCODE TABLES                                        14500
C                                                                          14505
C     SYS: M68SAM                                                          14510
C                                                                          14515
C     ENT: N/A                                                             14520
C     RTN: N/A                                                             14525
C                                                                          14530
C     FNC: SEARCH THE OPCODE TABLES, IF THE OPCODE IS                      14535
C          ITS INDEX IS RETURNED IN 'IOPIXB'. IF NOT                       14540
C          'IOPIXB' AND 'IOPCLS' (CLASS) ARE SET TO 1.                     14545
C*                                                                         14550
      COMMON ICOMON(232),IOPCOD(4),NPNAM(145),IOPCLS,IOPIXB                14555
      COMMON IOPBIN                                                        14560
      COMMON /A/ NOPCD(246),IOPBC2(51),IOPBC3(16)                          14565
      COMMON /A/ IOPBC4(116),IOPBC5(22)                                    14570
      DATA LASK0/10800/,LASK9/10809/                                       14575
      IOPCLS=0                                                             14580
      IOPIXB=1                                                             14585
      I=1                                                                  14590
100   N=NOPCD(I)                                                           14595
      I=I+1                                                                14600
      IF(IOPCOD(1).EQ.N) GO TO 110                                         14605
      IF(N.LT.LASK0.OR.N.GT.LASK9) GO TO 120                               14610
C***  NEW OPCODE CLASS FOUND                                               14615
      IOPCLS=IOPCLS+1                                                      14620
      IOPIXB=1                                                             14625
      IF(N.NE.LASK9) GO TO 100                                             14630
C***  UNDEFINED OPCODE (SET INDEX TO UNDEFINED OPCODE)                     14635
      IOPCLS=1                                                             14640
      GO TO 130                                                            14645
110   N=NOPCD(I)                                                           14650
      IF(IOPCOD(2).EQ.N) GO TO 130                                         14655
120   I=I+1                                                                14660
      IOPIXB=IOPIXB+1                                                      14665
      GO TO 100                                                            14670
C***  OPCODE FOUND (PLACE THE OPCODE BINARY IN IOPBIN)                     14675
130   IOPBIN=0                                                             14680
      RETURN                                                               14685
      END                                                                  14690
      SUBROUTINE MPAFSY (JNM,JRS,JSI)                                      14695
C+    NAM: MPAFSY  VER: 1.0  DAT: 10-01-74  CMP: ALL                       14700
C     PGM: FIND SYMBOL IN THE SYMBOL TABLE                                 14705
C                                                                          14710
C     SYS: M68SAM                                                          14715
C                                                                          14720
C                                                                          14725
C     ENT: JNM - 6 CHAR SYMBOL TO FIND IN SYSTEM'S R2 FORMAT               14730
C          JRS - N/A                                                       14735
C          JSI - N/A                                                       14740
C                                                                          14745
C     RTN: JNM - WORDS 1 TO 3 N/C                                          14750
C              - 4TH WORD EQUALS THE SYMBOLS VALUE IF FOUND                14755
C          JRS - 1 => SYMBOL FOUND                                         14760
C              - 2 => SYMBOL FOUND, BUT ERROR FLAG SET                     14765
C              - 3 => SYMBOL NOT IN THE TABLE                              14770
C          JSI - SYMBOL TABLE INDEX TO WORD 1 (IF FOUND)                   14775
C                                                                          14780
C     FNC: SEARCH THE SYMBOL TABLE FOR 6 CHARACTER SYMBOL                  14785
C          SYSTEM'S R2 ASCII FORMAT (WORDS 1-3 OF JNM)                     14790
C          AND RETURN IT'S VALUE IN WORD 4 IF FOUND.                       14795
C*                                                                         14800
      COMMON ICOMON(23),KC7F7F,KC80,ISIBUF(406),ISYM(813),LSYM             14805
      INTEGER JNM(4)                                                       14810
      DO 100 I=1,LSYM,4                                                    14815
      J=ISYM(I)                                                            14820
      IF(J.EQ.0) GO TO 110                                                 14825
      IF(JNM(1).NE.MPUAND(J,KC7F7F)) GO TO 100                             14830
      IF(JNM(2).NE.ISYM(I+1)) GO TO 100                                    14835
      IF(JNM(3).EQ.ISYM(I+2)) GO TO 120                                    14840
100   CONTINUE                                                             14845
      I=LSYM                                                               14850
C***  SYMBOL NOT FOUND                                                     14855
110   JRS=3                                                                14860
      GO TO 130                                                            14865
C***  SYMBOL WAS FOUND                                                     14870
120   JRS=1                                                                14875
      IF(MPUAND(J,KC80).NE.0) JRS=2                                        14880
      JNM(4)=ISYM(I+3)                                                     14885
130    JSI=I                                                               14890
      RETURN                                                               14895
      END                                                                  14900
      SUBROUTINE MPAGAM (JM,JSW)                                           14905
C+    NAM: MPAGAM  VER: 1.0  DAT: 10-01-74  CMP: ALL                       14910
C     PGM: GET ADDRESS MODE                                                14915
C                                                                          14920
C     SYS: M68SAM                                                          14925
C                                                                          14930
C     ENT: JM  - N/A                                                       14935
C          JSW - N/A                                                       14940
C                                                                          14945
C     RTN: JM  - 1=> DIRECT ADDRESS MODE                                   14950
C              - 2=> INDEXED ADDRESS MODE                                  14955
C              - 3=> IMMEDIATE ADDRESS MODE                                14960
C              - 4=> EXTENDED ADDRESS MODE                                 14965
C          JSW - SEE 'MPAOPR' FOR 'JSW' SETTINGS                           14970
C                                                                          14975
C     FNC: GET THE INSTRUCTIONS ADDRESS MODE                               14980
C*                                                                         14985
      COMMON ICOMON(377),IC,NC,ICSW,IPASS(4),IOPAN1,IPCT(7)                14990
      COMMON LIMA,LDRA,LSPCT(15),LSP,LASK(18),LX                           14995
      IOPAN1=0                                                             15000
      JSW=1                                                                15005
      JC=NC                                                                15010
      CALL MPUGNC(NC)                                                      15015
      JM=3                                                                 15020
      IF(LIMA.EQ.JC) GO TO 100                                             15025
      JM=1                                                                 15030
      IF(LDRA.EQ.JC) GO TO 100                                             15035
      IF(JC.EQ.LX.AND.NC.EQ.LSP) GO TO 130                                 15040
      JM=0                                                                 15045
      IC=IC-2                                                              15050
      CALL MPUGNC(NC)                                                      15055
100   CALL MPAOPR (IOPAN1,JSW)                                             15060
      GO TO (140,120,110,110,110,140),ICSW                                 15065
C***  ERROR IN OPERAND                                                     15070
110   JM=4                                                                 15075
      JSW=5                                                                15080
      GO TO 150                                                            15085
C***  OPERAND ENDS WITH A COMMA ','                                        15090
120   CALL MPUGNC(JC)                                                      15095
      CALL MPUGNC(NC)                                                      15100
      IF(JC.NE.LX.OR.NC.NE.LSP) GO TO 110                                  15105
C***  INDEXED ADDRESS MODE                                                 15110
130   JM=2                                                                 15115
140   IF(JM.NE.0) GO TO 150                                                15120
C***  MODE IS BY WORD LENGTH                                               15125
      JM=1                                                                 15130
      IF(JSW.NE.1) JM=4                                                    15135
150   RETURN                                                               15140
      END                                                                  15145
      SUBROUTINE MPAOPR (JV,JSW)                                           15150
C+    NAM: MPAOPR  VER: 1.2  DAT: 09-03-75  CMP: ALL                       15155
C     PGM: FORM THE OPERAND'S VALUE                                        15160
C                                                                          15165
C     SYS: M68SAM                                                          15170
C                                                                          15175
C     ENT: JV  - N/A                                                       15180
C          JSW - N/A                                                       15185
C                                                                          15190
C     RTN: JV  - VALUE OF THE OPERAND                                      15195
C          JSW - OPERAND VALUE STATUS:                                     15200
C              - 1=> 8 BIT VALUE                                           15205
C              - 2=> 16 BIT VALUE                                          15210
C              - 3=> VALUE OVERFLOWED 16 BITS                              15215
C              - 4=> UNDEFINED SYMBOLS IN THE OPERAND FIELD                15220
C              - 5=> SYNTAX ERROR IN THE FILED                             15225
C                                                                          15230
C     FNC: THE ROUTINE SCANS TO THE START OF THE OPERAND FIELD             15235
C          AND THAN FORMS THE VALUE OF THE OPERAND BY THE                  15240
C          OPERATIONIAL SIGNS IN THE FIELD, OPERATION ARE PER-             15245
C          FORM AS THEY ARE FOUND, (NO ORDER OTHER THAN THE                15250
C          SCANNING FROM LEFT TO RIGTH OF THE FIELD).                      15255
C                                                                          15260
C     REV: 1.2 - LSBH, LSBO AND LSBB CHECK FOR MPUFNO                      15265
C*                                                                         15270
      COMMON ICOMON(329),NAM(5),ISIMBF(44),NC,ICSW,IPASS                   15275
      COMMON IOPCLS(4),IPCT,ITERR(8),LSPCT,LSASC,NOPIB(14),LASK            15280
      COMMON LPOS,LCOMA,LMNS,LSLASH                                        15285
      JV=0                                                                 15290
      NS=1                                                                 15295
      JSW=1                                                                15300
      LEVS=1                                                               15305
      JBRSW=2                                                              15310
      GO TO 110                                                            15315
100   CALL MPUGNC (NC)                                                     15320
110   GO TO (120,150,160),LEVS                                             15325
C***  * * *  ' ' ',' 0-9 A-Z SPE                                           15330
120   GO TO (170,180,280,190,200,170),ICSW                                 15335
C***  SYNTAX ERROR                                                         15340
130   JSW=5                                                                15345
140   LEVS=2                                                               15350
150   GO TO (170,170,100,100,100,170),ICSW                                 15355
160   GO TO (170,170,130,130,200,170),ICSW                                 15360
C***  END OF THE OPERAND                                                   15365
170   IF(JBRSW.EQ.2.AND.JSW.EQ.1) JSW=5                                    15370
180   RETURN                                                               15375
C***  BUILD SYMBOL AND LOOK IT UP IN THE SYMBOL TABLE                      15380
190   CALL MPUBN8 (NAM)                                                    15385
      CALL MPAFSY (NAM,I,J)                                                15390
      N=NAM(4)                                                             15395
      JBRSW=1                                                              15400
C***  ERROR '222' A ERROR IN THE SYMBOL (SYMBOL WAS REDEFINED              15405
C***             OR A SYNTAX ERROR IN A 'EQU')                             15410
      IF(IPASS.EQ.2.AND.I.EQ.2) CALL MPAERR (222)                          15415
      IF(I.LT.3) GO TO 300                                                 15420
C***  ERROR '211' UNDEFINED SYMBOL                                         15425
      IF(IPASS.EQ.2) CALL MPAERR (211)                                     15430
      JSW=4                                                                15435
      GO TO 140                                                            15440
C***  SPECIAL CHARACTER                                                    15445
200   IF(NS.EQ.0) GO TO 210                                                15450
C***  A SIGN HAS BEEN DEFINED                                              15455
      IF(NC.NE.LSPCT) GO TO 210                                            15460
C***  USE CURRENT P COUNTER                                                15465
      N=IPCT                                                               15470
      GO TO 270                                                            15475
210   IF(NC.EQ.LPOS) GO TO 240                                             15480
      IF(NS.GT.1) GO TO 260                                                15485
      IF(NC.EQ.LMNS) GO TO 230                                             15490
      IF(NC.EQ.LASK) GO TO 220                                             15495
      IF(NC.NE.LSLASH) GO TO 260                                           15500
C***  OPERATION SIGN CHARACTER                                             15505
      NS=4                                                                 15510
      GO TO 250                                                            15515
220   NS=3                                                                 15520
      GO TO 250                                                            15525
230   NS=2                                                                 15530
      GO TO 250                                                            15535
240   NS=1                                                                 15540
250   LEVS=1                                                               15545
      GO TO 100                                                            15550
260   IF(NC.NE.LSASC) GO TO 280                                            15555
C***  ASCII CHARACTER                                                      15560
      CALL MPUGNC(N)                                                       15565
270   CALL MPUGNC(NC)                                                      15570
      GO TO 290                                                            15575
280   CALL MPUFNO(N,I)                                                     15580
      IF(I.EQ.2) GO TO 130                                                 15585
290   JBRSW=1                                                              15590
C***  END OF FIELD                                                         15595
300   IF(NS.EQ.0) GO TO 130                                                15600
      CALL MPUADR (JV,NS,N,JSW)                                            15605
      IF(JSW.GT.2) GO TO 140                                               15610
      NS=0                                                                 15615
      LEVS=3                                                               15620
      GO TO 110                                                            15625
      END                                                                  15630
      SUBROUTINE MPAOPT                                                    15635
C+    NAM: MPAOPT  VER: 1.0  DAT: 09-02-75  CMP: ALL                       15640
C     PGM: CROSS-ASSMEBLER OPTION DIRECTIVE SCAN ROUTINE                   15645
C                                                                          15650
C     SYS: M68SAM                                                          15655
C                                                                          15660
C     ENT: N/A                                                             15665
C     RTN: N/A                                                             15670
C                                                                          15675
C     FNC: SCAN THE 'OPT' OPERAND FIELD AN SET THE REQUESTED               15680
C          OPTIONS FOUND 1=> NO, 2=>YES.                                   15685
C*                                                                         15690
      COMMON ICOMON(244),ICSHF,IOTBUF(84),NAM(5),ISIMBF(44),NC             15695
      COMMON ICSW,IPASS(17),NOPT(2),NOPC(4),NOPCL,IXBASE,LSPSP             15700
      COMMON L8SP,L10,L16,LSP(14),LN,LO                                    15705
      EQUIVALENCE (N1,NAM(1)),(N2,NAM(2)),(N3,NAM(3))                      15710
      EQUIVALENCE (NOPBAS,NOPT(1))                                         15715
      INTEGER JPT(3)                                                       15720
100   IANS=2                                                               15725
      IF(NC.NE.LN) GO TO 110                                               15730
C***  'N' OR 'NO' => TURN THE OPTION OFF                                   15735
      IANS=1                                                               15740
      CALL MPUGNC(NC)                                                      15745
      IF(NC.EQ.LO) CALL MPUGNC(NC)                                         15750
110   CALL MPUBN8(NAM)                                                     15755
C***  LOOK-UP THE OPTION                                                   15760
      DO 120I=1,NOPCL,2                                                    15765
      IF(N1.EQ.NOPC(I).AND.N2.EQ.NOPC(I+1)) GO TO 180                      15770
120   CONTINUE                                                             15775
      N=N1/ICSHF                                                           15780
      DO 130I=1,NOPCL,2                                                    15785
      IF(N.EQ.NOPC(I)/ICSHF) GO TO 180                                     15790
130   CONTINUE                                                             15795
C***  ERROR '217' UNDEFINED OPTION OR SYNTAX IN OPTION FORMAT              15800
140   CALL MPAERR (217)                                                    15805
150   GO TO (220,160,170,100,170,220),ICSW                                 15810
C***  SKIP TO START OF THE NEXT FIELD                                      15815
160   CALL MPUGNC(NC)                                                      15820
      GO TO 150                                                            15825
170   CALL MPUGNC (NC)                                                     15830
      GO TO 140                                                            15835
180   NOPCMD=I/2+1                                                         15840
      IF(NOPCMD.EQ.1) GO TO 190                                            15845
      NOPT(NOPCMD)=IANS                                                    15850
      GO TO 150                                                            15855
C***  CHANGE DISPLAY NUMBER BASE                                           15860
190   N=N2                                                                 15865
      IF(N3.NE.LSPSP) N=N3                                                 15870
      I=8                                                                  15875
      IF(N.EQ.L8SP) GO TO 210                                              15880
      IF(N.EQ.L10) GO TO 200                                               15885
      IF(N.NE.L16) GO TO 140                                               15890
      I=I+6                                                                15895
200   I=I+2                                                                15900
210   NOPBAS=I                                                             15905
      IXBASE=I/5                                                           15910
      GO TO 150                                                            15915
220   RETURN                                                               15920
      END                                                                  15925
      SUBROUTINE MPAPRL (JDOSW)                                            15930
C+    NAM: MPAPRL  VER: 1.1  DAT: 09-03-75  CMP: ALL                       15935
C     PGM: PRINT AND OUTPUT THE SOURCE LIST                                15940
C                                                                          15945
C     SYS: M68SAM                                                          15950
C                                                                          15955
C      ENT: JDOSW - PRINT AND OUTPUT FUNCTION AS BIT SETTING:              15960
C                   1)  1=> LINE NUMBER                                    15965
C                   2)  2=> P COUNTER                                      15970
C                   3)  4=> OPCODE                                         15975
C                   4)  8=> OPERAND 1                                      15980
C                   5) 16=> OPERAND 2                                      15985
C                   6) 32=> STATEMENT                                      15990
C                   7) 64=> DATA BYTE 1                                    15995
C                   8) 128=> DATA BYTE 2                                   16000
C                   9) 256=> OUTPUT BINARY CODE                            16005
C                  10) 512=> PRINT IN PASS ONE                             16010
C                                                                          16015
C     RTN: JDOSW - N/C                                                     16020
C                                                                          16025
C     FNC: PRINT THE SOURCE LINE AS THE SETTING OF 'JDOSW'                 16030
C          AND THEN OUTPUT BINARY CODE TO THE MEMORY FILE.                 16035
C*                                                                         16040
      COMMON ICOMON(105),NB(121),INX,LABEL(5),IOPCOD(4)                    16045
      COMMON NPNAM(5),IPGLOL,IPGLEN(2),ICSHF,IOTBUF(89)                    16050
      COMMON ISIMBF(43),IC,NC,ICSW,IPASS,IOPCLS(2),IOPBIN                  16055
      COMMON IOPAN1,IPCT,ITERR,NRTSW,NSSCOL,NSOSCH,NAMSW(7)                16060
      COMMON NOPT(2),NOPC(5),IXBASE,LSPSP(4),LSP                           16065
      EQUIVALENCE (ISILN,ISIMBF(2)),(NOPBAS,NOPT(1))                       16070
      EQUIVALENCE (NOPLI,NOPT(2))                                          16075
      INTEGER JLDF(6),JSDF(6),JSSDF(3)                                     16080
      DATA JLDF(1)/3/,JLDF(2)/3/,JLDF(3)/2/,JLDF(4)/6/,JLDF(5)/5/          16085
      DATA JLDF(6)/4/,JSDF(1)/14/,JSDF(2)/13/,JSDF(3)/12/,JSDF(4)/18/      16090
      DATA JSDF(5)/17/,JSDF(6)/15/                                         16095
      DATA JSSDF(1)/25/,JSSDF(2)/23/,JSSDF(3)/20/                          16100
      JPP1SW=MPUBSM(JDOSW,10,1)                                            16105
      IF(JPP1SW.EQ.0.AND.IPASS.EQ.1) RETURN                                16110
      JDSW=JDOSW                                                           16115
      GO TO (350,100),NOPLI                                                16120
100   DO 110I=1,IPGLOL                                                     16125
      NB(I)=LSP                                                            16130
110   CONTINUE                                                             16135
      DO 340N=1,8                                                          16140
      I=MPUAND(JDSW,1)                                                     16145
      JDSW=JDSW/2                                                          16150
      IF (I.EQ.0) GO TO 340                                                16155
      I=0                                                                  16160
      K=0                                                                  16165
      J=IOPAN1                                                             16170
      GO TO (120,130,140,160,150,200,180,170),N                            16175
C***  FORMAT THE LINE NUMBER                                               16180
120   INX=1                                                                16185
      CALL MPUCNA (ISILN,10,5)                                             16190
      GO TO 340                                                            16195
C***  FORMAT THE P COUNTER (ADR)                                           16200
130   K=3                                                                  16205
      INX=7                                                                16210
      J=IPCT                                                               16215
      GO TO 190                                                            16220
C***  FORMAT THE OPCODE                                                    16225
140   J=IOPBIN                                                             16230
      GO TO 180                                                            16235
C***  FORMAT OPERAND 2                                                     16240
150   K=3                                                                  16245
C***  FORMAT OPERAND 1                                                     16250
160   I=3                                                                  16255
      GO TO 180                                                            16260
C***  FORMAT DATA BYTE 2                                                   16265
170   K=3                                                                  16270
C***  FORMAT DATA BYTE 1                                                   16275
180   I=I+IXBASE                                                           16280
      INX=JSDF(I)                                                          16285
190   K=K+IXBASE                                                           16290
      I=JLDF(K)                                                            16295
      CALL MPUCNA (J,NOPBAS,I)                                             16300
      GO TO 340                                                            16305
C                                                                          16310
C***  MOVE THE STATEMENT TO THE PRINT BUFFER                               16315
C                                                                          16320
200   INX=JSSDF(IXBASE)                                                    16325
      JSIX=INX                                                             16330
      ISIC=IC-1                                                            16335
      IC=0                                                                 16340
      IF(NRTSW.EQ.1) GO TO 310                                             16345
C***  MOVE THE LABEL FIELD                                                 16350
210   CALL MPUGNC(NC)                                                      16355
      IF(IC.LT.7) CALL MPUSNC(NC)                                          16360
      IF(ICSW.EQ.1.OR.ICSW.EQ.6) GO TO 220                                 16365
      GO TO 210                                                            16370
C***  FORMAT THE OPCODE                                                    16375
220   IF(INX-JSIX.LT.7) INX=JSIX+7                                         16380
      I=IOPCOD(1)/ICSHF                                                    16385
      CALL MPUSNC(I)                                                       16390
      I=IOPCOD(1)-I*ICSHF                                                  16395
      CALL MPUSNC(I)                                                       16400
      I=IOPCOD(2)/ICSHF                                                    16405
      CALL MPUSNC(I)                                                       16410
      INX=INX+1                                                            16415
      I=IOPCOD(2)-I*ICSHF                                                  16420
      CALL MPUSNC(I)                                                       16425
C***  FIND THE START OF THE NEXT FIELD                                     16430
      IC=NSSCOL-1                                                          16435
      LEVS=0                                                               16440
230   INX=INX+2                                                            16445
240   LEVS=LEVS+1                                                          16450
250   CALL MPUGNC(NC)                                                      16455
260   GO TO (270,280,280,270,300),LEVS                                     16460
270   IF(ICSW.EQ.1) GO TO 250                                              16465
      IF(ICSW.EQ.6) GO TO 330                                              16470
      LEVS=LEVS+1                                                          16475
      GO TO 260                                                            16480
280   IF(NRTSW.EQ.2) GO TO 300                                             16485
C***  MOVE THE OPERAND FIELD                                               16490
      IF(INX.GE.IPGLOL) GO TO 330                                          16495
      CALL MPUSNC(NC)                                                      16500
      IF(NRTSW.NE.4) GO TO 290                                             16505
C***  OPCODE 'FCC N,'                                                      16510
      NSOSCH=NSOSCH-1                                                      16515
      IF(NSOSCH.EQ.0) GO TO 230                                            16520
      GO TO 250                                                            16525
290   IF(LEVS.EQ.2) GO TO 240                                              16530
      IF(NSOSCH.EQ.NC) GO TO 230                                           16535
      GO TO 250                                                            16540
C***  MOVE THE COMMENT FIELD                                               16545
300   IF(INX-JSIX.LT.23) INX=JSIX+23                                       16550
      IC=IC-1                                                              16555
310   L=IPGLOL-INX                                                         16560
      DO 320I=1,L                                                          16565
      CALL MPUGNC(NC)                                                      16570
      CALL MPUSNC(NC)                                                      16575
320   CONTINUE                                                             16580
330   IC=ISIC                                                              16585
      CALL MPUGNC(NC)                                                      16590
340   CONTINUE                                                             16595
C***  PRINT THE LINE                                                       16600
      CALL MPAPA1 (2,NB,IPGLOL)                                            16605
      CALL MPAPAG(1)                                                       16610
C                                                                          16615
C***  STORE THE INSTRUCTION IN THE TAPE FILE                               16620
C                                                                          16625
350   IF(JPP1SW.EQ.1) GO TO 380                                            16630
      JDSW=JDOSW                                                           16635
      IF(MPUBSM(JDSW,9,1).EQ.0) GO TO 380                                  16640
      JADR=IPCT                                                            16645
      IF(MPUBSM(JDSW,3,1).EQ.0) GO TO 360                                  16650
      CALL MPUPTS (2,JADR,IOPBIN)                                          16655
      JADR=JADR+1                                                          16660
360   JV=IOPAN1                                                            16665
      IF(MPUBSM(JDSW,4,9).EQ.0) GO TO 370                                  16670
      IF(JV.LT.0) JV=256-IABS(JV)                                          16675
      CALL MPUPTS (2,JADR,JV)                                              16680
      GO TO 380                                                            16685
370   IF(MPUBSM(JDSW,5,9).EQ.0) GO TO 380                                  16690
      CALL MPUNEG(JV)                                                      16695
      CALL MPUXBY(JV, JV1, JV2)                                            16700
      CALL MPUPTS (2,JADR,JV1)                                             16705
      JADR=JADR+1                                                          16710
      CALL MPUPTS (2,JADR,JV2)                                             16715
380   RETURN                                                               16720
      END                                                                  16725
      SUBROUTINE MPAPSC                                                    16730
C+    NAM: MPAPSC  VER: 1.2  DAT: 08-30-75  CMP: ALL                       16735
C     PGM: DIRECTIVE OPCODES DECODE                                        16740
C                                                                          16745
C     SYS: M68SAM                                                          16750
C                                                                          16755
C     ENT: N/A                                                             16760
C     RTN: N/A                                                             16765
C                                                                          16770
C     FNC: DIRECTIVE OPCODE PROCCESSOR                                     16775
C                                                                          16780
C     REV: 1.2 - 'NS0SCH' CHANGED TO 'NSOSCH'                              16785
C*                                                                         16790
      COMMON ICOMON(227),LABEL(4),LABESW,IOPCOD(11),NPLCT                  16795
      COMMON ICSHF(133),IC,NC,ICSW,IPASS,IOPCLS,IOPIXB,IOPBIN              16800
      COMMON IOPAN1,IPCT,ITERR,NRTSW,NSSCOL,NSOSCH                         16805
      I=IOPIXB-4                                                           16810
C***  * * *  PAG ORG EQU FCB FCC FDB RMB SPC OPT                           16815
      GO TO (290,100,120,170,210,240,280,300,320),I                        16820
C                                                                          16825
C***  'ORG' ASSIGN PROGRAM COUNTER                                         16830
C                                                                          16835
100   CALL MPAOPR (IOPAN1,JOPRSW)                                          16840
      IF(JOPRSW.GT.2) GO TO 110                                            16845
      IPCT=IOPAN1                                                          16850
      JDOSW=35                                                             16855
      GO TO 340                                                            16860
C***  ERROR '216' SYNTAX ERROR IN DIRECTIVE'S OPERAND                      16865
110   CALL MPAERR (216)                                                    16870
      CALL MPAPRL (35)                                                     16875
      RETURN                                                               16880
C                                                                          16885
C***  'EQU' EQUATE SYMBOL                                                  16890
C                                                                          16895
120   CALL MPAFSY (LABEL,I,J)                                              16900
      IF(I.NE.1.AND.IPASS.EQ.1) RETURN                                     16905
      CALL MPAOPR (IOPAN1,JOPRSW)                                          16910
      IF(JOPRSW.LT.3.AND.I.EQ.1) GO TO 140                                 16915
C***  '213' NO LABEL, SYNTAX OR REDEFINED EQU DIRECTIVE                    16920
      CALL MPAERR (213)                                                    16925
C***  SET THE SYMBOL'S ERROR CODE BIT                                      16930
130   CALL MPASSY (LABEL,3,I)                                              16935
      GO TO 160                                                            16940
140   GO TO (150,160),IPASS                                                16945
C***  STORE THE SYMBOL VALUE (NO STORE IF SYMBOL IS IN ERR)                16950
150   IF(LABESW.EQ.2.AND.LABEL(4).NE.IOPAN1) GO TO 130                     16955
      LABEL(4)=IOPAN1                                                      16960
      CALL MPASSY (LABEL,2,LABESW)                                         16965
      RETURN                                                               16970
C***  PRINT THE SOURCE LINE                                                16975
160   CALL MPAPRL (161)                                                    16980
      RETURN                                                               16985
C                                                                          16990
C***  'FCB' FORM CONSTANT BYTE                                             16995
C                                                                          17000
170   I=355                                                                17005
180   CALL MPAOPR (IOPAN1,JOPRSW)                                          17010
      GO TO (200,190),IPASS                                                17015
C***  ERROR '210' BYTE OVERFLOW (> 255)                                    17020
190   IF(JOPRSW.EQ.2.OR.JOPRSW.EQ.3) CALL MPAERR (210)                     17025
C***  ERROR '214' SYNTAX ERROR IN FCB DIRECTIVE                            17030
      IF(JOPRSW.EQ.5) CALL MPAERR (214)                                    17035
      CALL MPAPRL(I)                                                       17040
      I=322                                                                17045
200   IPCT=IPCT+1                                                          17050
      IF(ICSW.NE.2) RETURN                                                 17055
      CALL MPUGNC(NC)                                                      17060
      GO TO 180                                                            17065
C                                                                          17070
C***  'FCC' FORM CONSTANT CHARACTERS                                       17075
C                                                                          17080
210   JSIC=IC                                                              17085
      NSOSCH=NC                                                            17090
      CALL MPUFNO(JV,I)                                                    17095
      IF(JV.EQ.0.AND.I.EQ.2.OR.ICSW.NE.2) GO TO 220                        17100
C***  'FCC N,TEXT'                                                         17105
      IF(I.NE.1.OR.JV.GT.255.OR.JV.LT.1) GO TO 110                         17110
      NRTSW=4                                                              17115
      NSOSCH=JV+IC+1-JSIC                                                  17120
      JSIC=IC                                                              17125
C***  'FCC /TEXT/'                                                         17130
220   I=355                                                                17135
      IC=JSIC                                                              17140
230   CALL MPUGNC(IOPAN1)                                                  17145
      IF(NRTSW.NE.4.AND.ICSW.EQ.6.OR.IOPAN1.EQ.NSOSCH) RETURN              17150
      CALL MPAPRL(I)                                                       17155
      I=322                                                                17160
      IPCT=IPCT+1                                                          17165
      IF(NRTSW.NE.4) GO TO 230                                             17170
      JV=JV-1                                                              17175
      IF(JV.EQ.0) RETURN                                                   17180
      GO TO 230                                                            17185
C                                                                          17190
C***  'FDB' FORM DOUBLE CONSTANT BYTE                                      17195
C                                                                          17200
240   I=419                                                                17205
250   CALL MPAOPR(IOPAN1,JOPRSW)                                           17210
      GO TO (270,260),IPASS                                                17215
C***  ERROR '215' SYNTAX ERROR IN FDB DIRECTIVE                            17220
260   IF(JOPRSW.EQ.3.OR.JOPRSW.EQ.5) CALL MPAERR (215)                     17225
      CALL MPAPRL(I)                                                       17230
      I=386                                                                17235
270   IPCT=IPCT+2                                                          17240
      IF(ICSW.NE.2) RETURN                                                 17245
      CALL MPUGNC(NC)                                                      17250
      GO TO 250                                                            17255
C                                                                          17260
C***  'RMB' RESERVE MEMORY BYTES                                           17265
C                                                                          17270
280   CALL MPAOPR (IOPAN1,JOPRSW)                                          17275
      IF(JOPRSW.GT.2) GO TO 110                                            17280
      CALL MPAPRL(163)                                                     17285
      IPCT=IPCT+IOPAN1                                                     17290
      RETURN                                                               17295
C                                                                          17300
C***  'PAGE' PLACE PAGE AT TOP OF THE NEXT PAGE (PASS 2 ONLY)              17305
C                                                                          17310
290   IF(IPASS.EQ.1) GO TO 330                                             17315
      IF(NPLCT.GT.8) CALL MPAPAG(0)                                        17320
      GO TO 330                                                            17325
C                                                                          17330
C***  'SPC' SPACE 'N' LINES                                                17335
C                                                                          17340
300   CALL MPAOPR (JV,JOPRSW)                                              17345
      IF(JOPRSW.GT.2) GO TO 110                                            17350
C***  STOP SPACING AT THE TOP OF A PAGE OR WHEN COUNT = 0                  17355
310   IF(JV.LT.1.OR.NPLCT.LT.5) GO TO 330                                  17360
      CALL MPAPRL (0)                                                      17365
      JV=JV-1                                                              17370
      GO TO 310                                                            17375
C                                                                          17380
C***  'OPT' OPTION RECORD                                                  17385
C                                                                          17390
320   CALL MPAOPT                                                          17395
      JDOSW=33                                                             17400
      GO TO 340                                                            17405
C***  CHECK FOR LABEL ON DIRECTIVES WHICH SHOULD NOT HAVE ANY              17410
330   JDOSW=0                                                              17415
340   IF(LABEL(1).EQ.0) GO TO 350                                          17420
C***  ERROR '223' DIRECTIVE SHOULD NOT HAVE A LABEL                        17425
      CALL MPAERR (223)                                                    17430
      LABEL(4)=0                                                           17435
      CALL MPASSY (LABEL,2,I)                                              17440
      CALL MPASSY (LABEL,3,I)                                              17445
      IF(JDOSW.EQ.0) JDOSW=33                                              17450
350   IF(JDOSW.NE.0) CALL MPAPRL (JDOSW)                                   17455
      RETURN                                                               17460
      END                                                                  17465
      SUBROUTINE MPASSY (JNAM,JDSW,JRSW)                                   17470
C+    NAM: MPASSY  VER: 1.0  DAT: 09-03-75  CMP: ALL                       17475
C     PGM: STORE INTO THE SYMBOL TABLE                                     17480
C                                                                          17485
C     SYS: M68SAM                                                          17490
C                                                                          17495
C                                                                          17500
C     ENT: JNAM - 4 WORD SYMBOL TO STORE                                   17505
C               - WORDS 1 TO 3 SYMBOL IN SYSTEM'S R2 FORMAT                17510
C               - WORD 4 SYMBOL'S VALUE                                    17515
C          JDSW - 1 => STORE THE SYMBOL                                    17520
C               - 2 => FIND THE SYMBOL AND CHANGE WORD 4                   17525
C               - 3 => SET THE SYMBOL'S DEFINATION ERROR FLAG              17530
C          JRSW - N/A                                                      17535
C                                                                          17540
C     RTN: JNAM - N/C                                                      17545
C          JDSW - N/C                                                      17550
C          JRSW - 1 => OPERATION COMPLETED OK                              17555
C               - 2 => SYMBOL IS IN THE SYMBOL TABLE                       17560
C               - 3 => ERROR, ERROR CODE BIT SET, SYMBOL TABLE             17565
C                      OVERFLOW, OR UNDEFINE SYMBOL (JDSW 2&3)             17570
C                                                                          17575
C     FNC: SEARCH THE SYMBOL TABLE FOR THE SYMBOL AND THEN                 17580
C          STORE THE SYMBOL OR CHANGE IT'S VALUE OR SET                    17585
C          IT'S ERROR FLAG.                                                17590
C*                                                                         17595
      COMMON ICOMON(23),KC7F7F,KC80,ISIBUF(405),NOSYM,ISYM(813)            17600
      COMMON LSYM                                                          17605
      INTEGER JNAM(4)                                                      17610
      JRSW=1                                                               17615
      JV=JNAM(4)                                                           17620
      CALL MPAFSY (JNAM,J,I)                                               17625
      GO TO (100,150,160),JDSW                                             17630
C***  STORE THE SYMBOL INTO THE SYMBOL TABLE                               17635
100   GO TO (110,170,120),J                                                17640
C***  SYMBOL IS IN THE TABLE                                               17645
110   IF(ISYM(I+3).EQ.JV) RETURN                                           17650
      JRSW=2                                                               17655
      RETURN                                                               17660
C***  THE SYMBOL IS NOT IN THE TABLE                                       17665
120   IF(I+4.LT.LSYM) GO TO 130                                            17670
C***  ERROR '221' SYMBOL TABLE OVERFLOW                                    17675
      CALL MPAERR (221)                                                    17680
      GO TO 170                                                            17685
C***  STORE THE SYMBOL                                                     17690
130   NOSYM=NOSYM+1                                                        17695
      ISYM(I)=JNAM(1)                                                      17700
      ISYM(I+1)=JNAM(2)                                                    17705
      ISYM(I+2)=JNAM(3)                                                    17710
      ISYM(I+4)=0                                                          17715
140   ISYM(I+3)=JV                                                         17720
      RETURN                                                               17725
C***  CHANGE THE SYMBOL'S VALUE (WORD 4)                                   17730
150   IF(J.EQ.1) GO TO 140                                                 17735
C***  SYMBOL IS IN ERROR OR NOT IN THE TABLE                               17740
      GO TO 170                                                            17745
C***  SET THE SYMBOL'S ERROR FLAG                                          17750
160   IF(J.EQ.3) GO TO 170                                                 17755
      ISYM(I)=MPUAND(ISYM(I),KC7F7F)+KC80                                  17760
      RETURN                                                               17765
170   JRSW=3                                                               17770
      RETURN                                                               17775
      END                                                                  17780
      SUBROUTINE MPUADR(JP1, JS, JP2, JSW)                                 17785
C+    NAM: MPUADR  VER: 1.1  DAT: 03-18-75  CMP: ALL                       17790
C     PGM: 16 BIT 2'S COMPLEMENT ADDER                                     17795
C                                                                          17800
C     SYS: ALL                                                             17805
C                                                                          17810
C     ENT: JP1 - OPERAND 1                                                 17815
C          JS  - FUNCTION TO PERFORM                                       17820
C                1 => ADD                                                  17825
C                2 => SUBTRACT                                             17830
C                3 => MULTIPLY                                             17835
C                4 => DIVIDE                                               17840
C          JP2 - OPERAND 2                                                 17845
C          JSW - N/A                                                       17850
C                                                                          17855
C     RTN: JP1 - RESULT OF OPERATION                                       17860
C          JS  - N/C                                                       17865
C          JP2 - N/C                                                       17870
C          JSW - STATUS OF OPERATION                                       17875
C                1 => 8 BITS                                               17880
C                2 => 16 BITS                                              17885
C                3 => OVERFLOW                                             17890
C                                                                          17895
C     FNC: PERFORMS REQUESTED ARITHMETIC FUNCTION AND DETERMINES           17900
C          BIT SIZE OF RESULT.  16 BIT OVERFLOW IS CHECKED.                17905
C                                                                          17910
C     REV: 1.1 - FIXED 16-BIT ARITH PROBLEM                                17915
C*                                                                         17920
      COMMON ICOMON(5),IBPWD                                               17925
C***  RESET OVERFLOW INDICATOR                                             17930
      CALL MPUOVF(JP1, 1)                                                  17935
      JSW = 1                                                              17940
      JOVF = 2                                                             17945
      IF (IBPWD .GT. 16) GO TO 100                                         17950
C                                                                          17955
C***  ADDER FOR MACHINES WITH WORDSIZE = 16 BITS.                          17960
C                                                                          17965
      GO TO (10, 20, 30, 40), JS                                           17970
10    JP1 = JP1 + JP2                                                      17975
      GO TO 50                                                             17980
20    JP1 = JP1 - JP2                                                      17985
      GO TO 50                                                             17990
30    CALL MPVMUL(JP1, JP2, JP1)                                           17995
      JOVF=3                                                               18000
      GO TO 50                                                             18005
40    CALL MPVDIV(JP1, JP2, JP1)                                           18010
50    IF (JP1 .LT. 0 .OR. JP1 .GE. 256) JSW = 2                            18015
      GO TO 160                                                            18020
C                                                                          18025
C***  ADDER FOR MACHINES WITH WORDSIZE > 16 BITS.                          18030
C                                                                          18035
100   GO TO (110, 120, 130, 140), JS                                       18040
110   JP1 = JP1 + JP2                                                      18045
      GO TO 150                                                            18050
120   JP1 = JP1 - JP2                                                      18055
      GO TO 150                                                            18060
130   JP1 = JP1*JP2                                                        18065
      JOVF=3                                                               18070
      GO TO 150                                                            18075
140   JP1 = JP1/JP2                                                        18080
150   IF (IABS(JP1) .GE. 256) JSW = 2                                      18085
C                                                                          18090
C***  TEST FOR OVERFLOW                                                    18095
C                                                                          18100
160   CALL MPUOVF(JP1, JOVF)                                               18105
      IF (JOVF .NE. 0) JSW = 3                                             18110
      RETURN                                                               18115
      END                                                                  18120
      SUBROUTINE MPUBN8 (JNAM)                                             18125
C+    NAM: MPUBN8  VER: 1.0  DAT: 04-28-75  CMP: ALL                       18130
C     PGM: BUILD 1 TO 8 CHARACTER ALPHA NUMERIC SYMBOL                     18135
C                                                                          18140
C     SYS: ALL                                                             18145
C                                                                          18150
C     ENT: JNAM - N/A                                                      18155
C                                                                          18160
C     RTN: JNAM - 1 TO 8 CHAR SYMBOL IN SYSTEM'S R2 FORMAT                 18165
C                                                                          18170
C     FNC: SCAN OUT FROM THE IMAGE BUFFER THE NEXT 1 TO 8                  18175
C          CHARACTER SYMBOL. THE SYMBOL CAN ONLY BE CHARACTERS             18180
C          'A' TO 'Z' '0' TO '9' AND MUST START WITH A ALPHA               18185
C          CHARACTER. THE UNFILLED CHARACTERS ARE SPACE FILLED.            18190
C          IF THE SYMBOL IS LONGER THAN 8 CHAR THE TRAILING                18195
C          CAHRACTERS ARE SKIPPED.                                         18200
C                                                                          18205
C     REV: N/A                                                             18210
C*                                                                         18215
      COMMON ICOMON(244),ICSHF,IOTBUF(133),NC,ICSW,IPASS(25)               18220
      COMMON LSPSP,L8SP(3),LSP                                             18225
      INTEGER JNAM(4)                                                      18230
      DO 10 I=1,4                                                          18235
10    JNAM(I)=LSPSP                                                        18240
      I=1                                                                  18245
      IF(ICSW.EQ.4) GO TO 30                                               18250
      RETURN                                                               18255
20    CALL MPUGNC (NC)                                                     18260
      IF(ICSW.LT.3.OR.ICSW.GT.4) RETURN                                    18265
30    JC=NC*ICSHF                                                          18270
      JNAM(I)=JC+LSP                                                       18275
      CALL MPUGNC (NC)                                                     18280
      IF(ICSW.LT.3.OR.ICSW.GT.4) RETURN                                    18285
      JNAM(I)=JC+NC                                                        18290
      I=I+1                                                                18295
      IF(I.LT.5) GO TO 20                                                  18300
C     SKIP TO THE END OF THE SYMBOL                                        18305
40    CALL MPUGNC (NC)                                                     18310
      IF(ICSW.LT.3.OR.ICSW.GT.4) RETURN                                    18315
      GO TO 40                                                             18320
      END                                                                  18325
      SUBROUTINE MPUFNO (JN,JER)                                           18330
C+    NAM: MPUFNO  VER: 1.3  DAT: 09-08-75  CMP: ALL                       18335
C     PGM: FORM A INTEGER NUMBER FROM THE IMAGE BUFFER                     18340
C                                                                          18345
C     SYS: ALL                                                             18350
C                                                                          18355
C     ENT: JN  - N/A                                                       18360
C          JER - N/A                                                       18365
C                                                                          18370
C     RTN: JN  - INTEGER NUMBER THAT WAS SCANNED FROM THE BUFFER           18375
C          JER - 1 => NO ERROR, OK                                         18380
C              - 2 => SYNTAX ERROR IN THE NUMBER                           18385
C                                                                          18390
C     FNC: THE ROUTINE WILL START THE SCAN AT THE PRESENT CHAR             18395
C          IN THE 'NC' BUFFER AND SCAN OUT THE NEXT INTEGER                18400
C          NUMBER. THE NUMBER MAY BE IN BASE BINARY, OCTAL,                18405
C          DECIMAL OR HEX. SEE FORMAT. THE SCAN WILL STOP AT               18410
C          THE NEXT NON-NUMERIC CHAR. THE CHAR WILL BE RETURNED            18415
C          IN THE 'NC' BUFFER.                                             18420
C                                                                          18425
C     GEN: FORMAT:                                                         18430
C          %N OR NB => BINARY (N = 0-1)                                    18435
C          @N NQ OR NO => OCTAL (N = 0-7)                                  18440
C          &N OR N => DECIMAL (N = 0-9)                                    18445
C          $N OR 0NH => HEX (N = 0-9 OR A-F)                               18450
C                                                                          18455
C     REV: 1.1 - BASE CHAR WITHOUT A DIGIT ARE FLAGGED AS ERRORS           18460
C          1.2 - FIXED 16-BIT ARITH PROBLEM                                18465
C          1.3 - DECIMAL SELECTED & ADDED, 'ND' DELETED.                   18470
C*                                                                         18475
      COMMON ICOMON(378),NC,ICSW,IPASS(16),NOPIB,NOPT(18),L0               18480
      COMMON L1(3),LB,LD,LF,LH,LN,LO,LQ                                    18485
      INTEGER JSBC(4),JBTB(4),JDBF(20)                                     18490
C***  * * * '%'         '@'         '&'         '$'                        18495
      DATA JSBC(1)/37/,JSBC(2)/64/,JSBC(3)/38/,JSBC(4)/36/                 18500
      DATA JBTB(1)/02/,JBTB(2)/08/,JBTB(3)/10/,JBTB(4)/16/                 18505
      JN=0                                                                 18510
      JSB=0                                                                18515
      JDIX=0                                                               18520
      JER=1                                                                18525
      JNSB=1                                                               18530
      JLEVEL=1                                                             18535
100   JNCSW=1                                                              18540
      GO TO (120,170),JLEVEL                                               18545
C***  * * *   SP  ,  0-9 A-Z SPC END                                       18550
120   GO TO (150,150,160,150,130,150),ICSW                                 18555
130   JLEVEL=2                                                             18560
      DO 140JSB=1,4                                                        18565
      IF(NC.EQ.JSBC(JSB)) GO TO 235                                        18570
140   CONTINUE                                                             18575
C***  ILLEGAL CHAR IN THE NUMBER FIELD                                     18580
150   JER=2                                                                18585
      RETURN                                                               18590
C***  BUILD NUMBER (LEVEL 2)                                               18595
160   JLEVEL=2                                                             18600
C***  * * *   SP  ,  0-9 A-Z SPC END                                       18605
170   GO TO (280,280,180,240,280,280),ICSW                                 18610
C***  CHAR IS '0' TO '9'                                                   18615
180   N=NC-L0                                                              18620
      GO TO (190,200,210,220),JNSB                                         18625
190   IF(N.GT.1) JNSB=2                                                    18630
200   IF(N.GT.7) JNSB=3                                                    18635
210   IF(N.GT.9) JNSB=4                                                    18640
220   IF(JDIX.GT.19) GO TO 230                                             18645
      JDIX=JDIX+1                                                          18650
      JDBF(JDIX)=N                                                         18655
230   GO TO (235,100),JNCSW                                                18660
235   CALL MPUGNC (NC)                                                     18665
      GO TO 100                                                            18670
C***  CHAR IS 'A' TO 'Z'                                                   18675
240   JC=NC                                                                18680
      JNCSW=2                                                              18685
      CALL MPUGNC(NC)                                                      18690
      IF(JC.GT.LF) GO TO 250                                               18695
      N=JC-55                                                              18700
      IF(JNSB.NE.1) GO TO 220                                              18705
      IF(JC.NE.LB.OR.JSB.NE.0) GO TO 210                                   18710
      IF(ICSW.EQ.3.OR.ICSW.EQ.4) GO TO 210                                 18715
250   J=1                                                                  18720
      IF(JC.EQ.LB) GO TO 270                                               18725
      IF(JC.EQ.LQ.OR.JC.EQ.LO) GO TO 260                                   18730
      IF(JC.NE.LH) GO TO 150                                               18735
      J=J+2                                                                18740
260   J=J+1                                                                18745
270   IF(JSB.EQ.0) JSB=J                                                   18750
      IF(ICSW.EQ.3.OR.ICSW.EQ.4.OR.J.NE.JSB) GO TO 150                     18755
C***  END OF THE NUMBER FIELD                                              18760
280   IF(JSB.NE.0) GO TO 290                                               18765
      JSB=NOPIB/5+1                                                        18770
      IF(JSB.LT.JNSB) JSB=JNSB                                             18775
290   IF(JSB.LT.JNSB) GO TO 150                                            18780
      IF(JDIX.EQ.0.OR.JDIX.GT.19) GO TO 150                                18785
      JBASE=JBTB(JSB)                                                      18790
      DO 300J=1,JDIX                                                       18795
      CALL MPVMUL (JN,JBASE,JN)                                            18800
      JN=JN+JDBF(J)                                                        18805
300   CONTINUE                                                             18810
      RETURN                                                               18815
      END                                                                  18820
      SUBROUTINE MPUGNC (JC)                                               18825
C+    NAM: MPUGNC  VER: 1.1  DAT: 08-12-74  CMP: ALL                       18830
C     PGM: GET NEXT CHARACTER FROM IMAGE BUFFER                            18835
C                                                                          18840
C     SYS: ALL                                                             18845
C                                                                          18850
C     ENT: JC - N/A                                                        18855
C                                                                          18860
C     RTN: JC - NEXT CHARACTER FORM THE IMAGE BUFFER                       18865
C                                                                          18870
C     FNC: GET THE NEXT CHARACTER AND RETURN IT IN 'NC' AND 'JC'           18875
C          ALSO SET 'ICSW' FOR TYPE OF CHARACTER. 'IC=IC+1'                18880
C                                                                          18885
C     GEN: ICSW IS SET AS FOLLOWS FOR THE CHARACTER RETURNED:              18890
C          ICSW IS SET TO)                                                 18895
C          1 => SPACE ' '                                                  18900
C          2 => COMMA ','                                                  18905
C          3 => '0' - '9'                                                  18910
C          4 => 'A' - 'Z'                                                  18915
C          5 => SPECIAL CHAR                                               18920
C          6 => END OF STATEMENT                                           18925
C          NOTE: IF THE CHAR. IS NOT WITHIN 20 TO 60 HEX ICSW=1            18930
C                                                                          18935
C     REV: 1.1 - TRAILING SPACES ARE NOT RETURNED                          18940
C*                                                                         18945
      COMMON ICOMON(244),ICSHF,IOTBUF(89),ISIMBF(43),IC,NC,ICSW            18950
      COMMON IPASS(29),LSP,LASK(2),LCOMA,LMNS(2),L0,L1,L9,LA               18955
      COMMON LB(10),LZ                                                     18960
      INTEGER IMAGE(40)                                                    18965
      EQUIVALENCE (IMAGE(1),ISIMBF(4))                                     18970
      IF(IC.LE.ISIMBF(1)*2-6) GO TO 10                                     18975
      NC=LSP                                                               18980
      JC=LSP                                                               18985
      ICSW=6                                                               18990
      RETURN                                                               18995
10    I=IC/2                                                               19000
      IW=IMAGE(I+1)                                                        19005
      I=IC-I*2                                                             19010
      IC=IC+1                                                              19015
      NC=IW/ICSHF                                                          19020
      IF(I.NE.0) NC=IW-(NC*ICSHF)                                          19025
      JC=NC                                                                19030
      IF(NC.NE.LSP) GO TO 20                                               19035
C***  CHAR IS A ' ' (SPACE)                                                19040
15    ICSW=1                                                               19045
      RETURN                                                               19050
20    IF(NC.NE.LCOMA) GO TO 30                                             19055
C***  CHAR IS A ',' (COMMA)                                                19060
      ICSW=2                                                               19065
      RETURN                                                               19070
30    IF(NC.LT.L0.OR.NC.GT.L9) GO TO 40                                    19075
C***  **   CHAR IS '0' - '9'                                               19080
      ICSW=3                                                               19085
      RETURN                                                               19090
40    IF(NC.LT.LA.OR.NC.GT.LZ) GO TO 50                                    19095
C***  CHAR IS 'A' - 'Z'                                                    19100
      ICSW=4                                                               19105
      RETURN                                                               19110
50    IF(NC.LT.LSP.OR.NC.GT.95) GO TO 15                                   19115
      ICSW=5                                                               19120
      RETURN                                                               19125
      END                                                                  19130
      SUBROUTINE MPUPIB (JB,JLSW)                                          19135
C+    NAM: MPUPIB  VER: 1.4  DAT: 09-08-75  CMP: ALL                       19140
C     PGM: PACK SOURCE IMAGE BUFFER                                        19145
C                                                                          19150
C     SYS: ALL                                                             19155
C                                                                          19160
C     ENT: JB   - 80 WORD BUFFER IN COMPUTER'S A1 FORMAT TO BE             19165
C                 CONVERTED AND PACKED                                     19170
C          JLSW - 1 => NO LINE NUMBERS IN THE BUFFER                       19175
C               - 2 => LINE NUMBER IN THE BUFFER                           19180
C                                                                          19185
C     RTN: JB   - N/C                                                      19190
C          JLSW - SET TO 1 IF THE 1ST CHARACTER OF THE BUFFER              19195
C                 'JB' WAS NOT WITHIN THE RANGE '0' TO '9'                 19200
C                                                                          19205
C     FNC: CONVERT THE BUFFER 'JB' FROM THE COMPUTER'S A1                  19210
C          FORMAT AND THEN REMOVE THE LINE NUMBER IF IT HAS                19215
C          ONE (CONVERTING IT TO BINARY) AND PACK THE BUFFER               19220
C          INTO THE SYSTEM'S R2 FORMAT. THE OUTPUT IS A PACKED             19225
C          'ISIMBF(43)' BUFFER IN THE FOLLOWING FORMAT:                    19230
C                                                                          19235
C          WORD 1 NUMBER OF WORDS+3 TO END OF TEXT (TRAILING               19240
C                 SPACES ARE NOT INCLUDED).                                19245
C          WORD 2 BINARY LINE NUMBER OF THE RECORD                         19250
C          WORD 3 1ST CHARACTER IN SYSTEM'S R1 FORMAT IF THE               19255
C                 RECORD'S LINE NUMBER WAS NOT FOLLOWED BY A               19260
C                 SPACE. OTHERWISE IT IS A SPACE.                          19265
C          WORDS 4 TO 43 ARE THE TEXT OF THE RECORD IN SYSTEM'S            19270
C                 R2 FORMAT.                                               19275
C                                                                          19280
C     GEN: THE 1ST CHARACTER IS RETURN BY A CALL TO 'MPUGNC'               19285
C          AFTER THE BUFFER IS PACKED UP. THE 1ST CHARACTER                19290
C          AFTER THE LINE NUMBER IS CHARACTER 1 IF IT IS NOT               19295
C          A SPACE, OTHERWISE IT IS THE 2ND ONE                            19300
C                                                                          19305
C     REV: 1.1 - 1ST CHAR RETURNED IN 'ISIMBF(3)'                          19310
C          1.2 - IF NO LINE 'ISILN' IS NOT CHANGED, MPURA1                 19315
C                PLACES THE LINE NUMBER IN 'ISILN'                         19320
C          1.3 - BLANK LINE WILL BE 2 SPACE LONG                           19325
C          1.4 - FIX 16-BIT ARITH PROBLEM                                  19330
C*                                                                         19335
      COMMON ICOMON(4),LLSPSP,IBPWD(239),ICSHF,IOTBUF(89)                  19340
      COMMON ISIMBF(43),IC,NC,ICSW(26),LSPSP,L8SP(3),LSP                   19345
      COMMON LASK(5),L0,L1,L9                                              19350
      EQUIVALENCE (ISILN,ISIMBF(2))                                        19355
      INTEGER JB(80)                                                       19360
      CALL MPUCVC (JB(1),JC)                                               19365
      IF(JC.LT.L0.OR.JC.GT.L9) JLSW=1                                      19370
      ISIMBF(3)=LSP                                                        19375
      IF(JLSW.EQ.2) ISIMBF(3)=JC                                           19380
      JULSW=1                                                              19385
      N=0                                                                  19390
      J=3                                                                  19395
      ISIMBF(1)=4                                                          19400
      DO 200 I=1,80                                                        19405
      JC=LSP                                                               19410
      JW=JB(I)                                                             19415
      IF(JW.NE.LLSPSP) CALL MPUCVC (JW,JC)                                 19420
      GO TO (120,160,170),JULSW                                            19425
120   GO TO (160,140),JLSW                                                 19430
C***  BUILD THE LINE NUMBER                                                19435
140   IF(JC.LT.L0.OR.JC.GT.L9) GO TO 150                                   19440
      CALL MPVMUL(N, 10, N)                                                19445
      N = N + (JC - L0)                                                    19450
      GO TO 200                                                            19455
C***  SAVE THE LINE NUMBER AND THE 1ST CHAR. AFTER IT                      19460
150   ISILN=N                                                              19465
      ISIMBF(3)=JC                                                         19470
      IF(JC.EQ.LSP) GO TO 180                                              19475
160   J=J+1                                                                19480
      ISIMBF(J)=JC*ICSHF+LSP                                               19485
      JULSW=3                                                              19490
      GO TO 190                                                            19495
170   ISIMBF(J)=ISIMBF(J)-LSP+JC                                           19500
180   JULSW=2                                                              19505
C***  SAVE THE END OF IMAGE INDEX                                          19510
190   IF(JC.NE.LSP) ISIMBF(1)=J                                            19515
200   CONTINUE                                                             19520
C***  SPACE FILL THE REMAINING PART OF THE BUFFER                          19525
210   J=J+1                                                                19530
      IF(J.GT.43) GO TO 220                                                19535
      ISIMBF(J)=LSPSP                                                      19540
      GO TO 210                                                            19545
C***  GET THE 1ST CHARACTER AND RETURN IT                                  19550
220   IC=0                                                                 19555
      CALL MPUGNC(NC)                                                      19560
      RETURN                                                               19565
      END                                                                  19570
      SUBROUTINE MPUPTS (JDSW,JADR,JBYT)                                   19575
C+    NAM: MPUPTS  VER: 1.0  DAT: 09-03-75  CMP: ALL                       19580
C     PGM: MEMORY OUTPUT TO PAPER TAPE FILE SETUP ROUTINE                  19585
C                                                                          19590
C     SYS: ALL                                                             19595
C                                                                          19600
C     ENT: JDSW - 1=> OPEN THE ROUINTE                                     19605
C               - 2=> STORE BYTE INTO PT BUFFER                            19610
C               - 3=> CLOSE THE ROUTINE                                    19615
C          JADR - IF JDSW=2, ADDRESS OF THE BYTE TO STORE                  19620
C          JBYT - IF JDSW=2, BYTE TO STORE AT ADDRESS JADR                 19625
C                                                                          19630
C     RTN: JDSW - N/C                                                      19635
C          JADR - N/C                                                      19640
C          JBYT - N/C                                                      19645
C                                                                          19650
C     FNC: THIS ROUTINE IS USED TO SETUP AND THEN OUTPUT                   19655
C          ONE BYTE AT A TIME TO THE 'IOTBUF' BUFFER                       19660
C          THE ROUTINE WILL OUTPUT THE RECORD WHEN IT IS FULL              19665
C          AND ALSO CHANGE BYTE ADDRESSES IF THE NEXT                      19670
C          ADDRESS IS NOT EQUAL TO 'JADR'. THE LAST CALL                   19675
C          (JDSW=3) WILL OUTPUT THE LAST RECORD AND END OF                 19680
C          P.T. FILE RECORD (S9).                                          19685
C                                                                          19690
C     REV: N/A                                                             19695
C*                                                                         19700
      COMMON ICOMON(326),IOTSW,IOTCKS,NOTADR,NAM(86),L0,L1,L9              19705
      COMMON LA(2),LD,LF,LH,LN(3),LR                                       19710
      GO TO (100,200,300),JDSW                                             19715
C                                                                          19720
C***  OPEN THE ROUTINE BY OUTPUTTING 'HDR' RECORD                          19725
C                                                                          19730
100   CALL MPUPTW (1,L0)                                                   19735
      CALL MPUPTW (2,0)                                                    19740
      CALL MPUPTW (3,LH)                                                   19745
      CALL MPUPTW (3,LD)                                                   19750
      CALL MPUPTW (3,LR)                                                   19755
      CALL MPUPTW (4,0)                                                    19760
      RETURN                                                               19765
C                                                                          19770
C***  STORE THE BYTE INTO THE P.T. BUFFER                                  19775
C                                                                          19780
200   GO TO (220,210),IOTSW                                                19785
C***  RECORD CONTAINS PART OF A RECORD                                     19790
210   IF(JADR.EQ.NOTADR.AND.MPUAND(NOTADR,15).NE.0) GO TO 230              19795
C***  OUTPUT THE LAST RECORD                                               19800
      CALL MPUPTW (4,0)                                                    19805
C***  START OF A NEW RECORD                                                19810
220   NOTADR=JADR                                                          19815
      CALL MPUPTW (1,L1)                                                   19820
      CALL MPUPTW (2,NOTADR)                                               19825
C***  SET THE 'OT' BUFFER SW 2=> BUFFER CONTAINS A RECORD                  19830
      IOTSW=2                                                              19835
C***  STORE THE BYTE INTO THE P.T. RECORD BUFFER                           19840
230   CALL MPUPTW (3,JBYT)                                                 19845
      NOTADR=NOTADR+1                                                      19850
      RETURN                                                               19855
C                                                                          19860
C***  CLOSE THE ROUTINE, OUTPUT THE 'S9' RECORD                            19865
C                                                                          19870
300   IF(IOTSW.NE.1) CALL MPUPTW (4,0)                                     19875
      CALL MPUPTW (1,L9)                                                   19880
      CALL MPUPTW (2,0)                                                    19885
      CALL MPUPTW (4,0)                                                    19890
      RETURN                                                               19895
      END                                                                  19900
      SUBROUTINE MPUPTW (JSW,JAD)                                          19905
C+    NAM: MPUPTW  VER: 1.2  DAT: 04-18-75  CMP: ALL                       19910
C     PGM: BUILD AND WRITE OUT PAPER TAPE FILE RECORDS                     19915
C                                                                          19920
C     SYS: ALL                                                             19925
C                                                                          19930
C     ENT: JSW - 1=> INITIALIZE, 'LX' WHERE: X IS JAD                      19935
C              - 2=> STORE P.T. RECORD'S 1ST BYTE ADDRESS                  19940
C              - 3=> STORE 8 BIT BYTE                                      19945
C              - 4=> END OF RECORD, OUTPUT IT                              19950
C          JAD - CHAR, ADDRESS OR BYTE TO BE STORED IN THE                 19955
C                P.T. FILE RECORD.                                         19960
C                                                                          19965
C     RTN: JSW - N/C                                                       19970
C          JAD - N/C                                                       19975
C                                                                          19980
C     FNC: BUILD A P.T. FILE RECORD A BYTE AT A TIME AND                   19985
C          OUTPUT THE RECORD TO THE 'OT' FILE.                             19990
C                                                                          19995
C     REV: 1.0 - JCKSUM CHANGED TO ITOCKS AND IS IN COMMON                 20000
C          1.1 - FIX FOR 16-BIT ARITH PROBLEMS                             20005
C          1.2 - ADDED 2 LEADING SPACES TO EACH OT RECORD                  20010
C*                                                                         20015
      COMMON ICOMON(3),LUOT,LLSPSP(18),KCFF,KC7F7F(82),NB(121)             20020
      COMMON INX,LABEL(18),IOTBUF(80),IOTINX,IOTSW,IOTCKS                  20025
      COMMON NOTADR(99),LS                                                 20030
      GO TO (100,200,300,400),JSW                                          20035
C***  INITIALIZE THE BUFFER WITH 'SX'                                      20040
100   IOTBUF(1)=LS                                                         20045
      IOTBUF(2)=JAD                                                        20050
      RETURN                                                               20055
C***  STORE THE ADDRESS OF THE 1ST BYTE OF THE RECORD                      20060
200   CALL MPVDIV(JAD, 256, IOTCKS)                                        20065
      IOTCKS = IOTCKS + MPUAND(JAD, KCFF)                                  20070
      L=4                                                                  20075
      IOTINX=4                                                             20080
      GO TO 310                                                            20085
C***  STORE THE NEXT BYTE IN THE RECORD                                    20090
300   IOTCKS=IOTCKS+JAD                                                    20095
      L=2                                                                  20100
310   INX=0                                                                20105
      CALL MPUCNA (JAD,16,L)                                               20110
      DO 320 J=1,L                                                         20115
      IOTINX=IOTINX+1                                                      20120
      IOTBUF(IOTINX)=NB(J)                                                 20125
320   CONTINUE                                                             20130
      RETURN                                                               20135
C***  END OF THE RECORD, ADD THE BYTE COUNT AND CHECKSUM                   20140
400   J=(IOTINX-2)/2                                                       20145
      IOTCKS=KCFF-MPUAND(IOTCKS+J,KCFF)                                    20150
      INX=0                                                                20155
      CALL MPUCNA (J,16,2)                                                 20160
      IOTBUF(3)=NB(1)                                                      20165
      IOTBUF(4)=NB(2)                                                      20170
      CALL MPUCNA (IOTCKS,16,2)                                            20175
      IOTINX=IOTINX+1                                                      20180
      IOTBUF(IOTINX)=NB(3)                                                 20185
      IOTINX=IOTINX+1                                                      20190
      IOTBUF(IOTINX)=NB(4)                                                 20195
      CALL MPUCA1 (IOTBUF,IOTINX)                                          20200
      WRITE (LUOT,411) (IOTBUF(J),J=1,IOTINX)                              20205
C***  RESET 'OT' BUFFER SWITCH, 1=> BUFFER EMPTY                           20210
      IOTSW=1                                                              20215
      RETURN                                                               20220
411   FORMAT(2X, 78A1)                                                     20225
      END                                                                  20230
      SUBROUTINE MPUXBY(JWORD, JBYTE1, JBYTE2)                             20235
C+    NAM: MPUXBY  VER: 1.2  DAT: 03-18-75  CMP: ALL                       20240
C     PGM: EXTRACT UPPER AND LOWER BYTES FROM WORD                         20245
C                                                                          20250
C     SYS: ALL                                                             20255
C                                                                          20260
C     ENT: JWORD  - WORD FROM WHICH BYTES ARE EXTRACTED                    20265
C          JBYTE1 - N/A                                                    20270
C          JBYTE2 - N/A                                                    20275
C     RTN: JWORD  - NC/                                                    20280
C          JBYTE1 - CONTAINS UPPER BYTE OF JWORD (RIGHT JUSTIFIED)         20285
C          JBYTE2 - CONTAINS LOWER BYTE OF JWORD (RIGHT JUSTIFIED)         20290
C                                                                          20295
C     FNC: EXTRACTS UPPER AND LOWER BYTES FROM JWORD                       20300
C                                                                          20305
C     REV 1.1 - AND USE ON LOWER 8 BITS                                    20310
C         1.2 - IMPLEMENTED MPVDIV TO EXTRACT UPPER BYTE                   20315
C*                                                                         20320
      COMMON ICOMON(22),KCFF                                               20325
      CALL MPVDIV(JWORD, 256, JBYTE1)                                      20330
      JBYTE2 = MPUAND(JWORD,KCFF)                                          20335
      RETURN                                                               20340
      END                                                                  20345
      SUBROUTINE MPUCA2(JLST, JN)                                          20350
C+    NAM: MPUCA2  VER: 1.0  DAT: 08-28-74  CMP: PDP-11                    20355
C     PGM: CONVERT TO A2 FORMAT                                            20360
C                                                                          20365
C     SYS: ALL                                                             20370
C                                                                          20375
C     ENT: JLST - ARRAY CONTAINING THE CHARACTERS, 2 PER WORD              20380
C                 RIGHT JUSTIFIED.                                         20385
C          JN   - NUMBER OF WORDS IN ARRAY                                 20390
C                                                                          20395
C     RTN: JLST - ARRAY CONTAINING CHARACTERS, 2 PER WORD                  20400
C                 IN COMPUTER'S A2 FORMAT.                                 20405
C          JN   - N/C                                                      20410
C                                                                          20415
C     FNC: CONVERT THE CHARACTERS IN ARRAY 'JLST' TO COMPUTER'S            20420
C          A2 FORMAT.                                                      20425
C                                                                          20430
C     GEN:                                                                 20435
C                                                                          20440
C     REV: N/A                                                             20445
C*                                                                         20450
      BYTE JLST(160), HOLD                                                 20455
      JN2 = JN*2                                                           20460
      DO 10 I = 1, JN2, 2                                                  20465
      HOLD = JLST(I)                                                       20470
      JLST(I) = JLST(I+1)                                                  20475
      JLST(I+1) = HOLD                                                     20480
10    CONTINUE                                                             20485
      RETURN                                                               20490
      END                                                                  20495
      SUBROUTINE MPUCVC(JW, JC)                                            20500
C+    NAM: MPUCVC  VER: 1.1  DAT: 04-01-75  CMP: PDP-11                    20505
C     PGM: CONVERT CHARACTER                                               20510
C                                                                          20515
C     ENT: JW - CHARACTER TO BE CONVERTED                                  20520
C          JC - N/A                                                        20525
C                                                                          20530
C     RTN: JW - N/C                                                        20535
C          JC - CONVERTED CHARACTER                                        20540
C                                                                          20545
C     FNC: CONVERT THE CHARACTER IN JW (SYSTEM'S A1 FORMAT) TO             20550
C          1 ASCII CHARACTER RIGHT JUSTIFIED IN JC.                        20555
C                                                                          20560
C     GEN:                                                                 20565
C                                                                          20570
C     REV: 1.1 - CONVERT LOWER CASE TO UPPER CASE                          20575
C*                                                                         20580
      JC = JW .AND. 127                                                    20585
      IF (JC .LT. 32) GO TO 10                                             20590
      IF (JC .LT. 96) GO TO 20                                             20595
      JC = JC - 32                                                         20600
      IF (JC .LT. 96) GO TO 20                                             20605
C***  CONVERT CHARACTER TO A SPACE                                         20610
10    JC = 32                                                              20615
20    RETURN                                                               20620
      END                                                                  20625
      SUBROUTINE MPUOVF(JNBR, JOVF)                                        20630
C+    NAM: MPUOVF  VER: 1.0  DAT: 01-27-75  CMP: PDP-11                    20635
C     PGM: OVERFLOW TEST                                                   20640
C                                                                          20645
C     SYS: ALL                                                             20650
C                                                                          20655
C     ENT: JNBR - NUMBER WHICH IS TESTED FOR OVERFLOW                      20660
C          JOVR - FUNCTION TO PERFORM                                      20665
C                 1 => RESET OVERFLOW INDICATOR                            20670
C                 2 => TEST FOR OVERFLOW FOR +, -, AND /                   20675
C                 3 => TEST FOR OVERFLOW FOR *                             20680
C                                                                          20685
C     RTN: JNBR - N/C                                                      20690
C          JOVF - 0 => NO OVERFLOW                                         20695
C                 1 => OVERFLOW                                            20700
C                                                                          20705
C     FNC: CHECKS FOR OVERFLOW ON LAST ARITHMETIC OPERATION                20710
C*                                                                         20715
      JOVF = 0                                                             20720
      RETURN                                                               20725
      END                                                                  20730
      SUBROUTINE MPAM2                                                     20735
C+    NAM: MPAM2   VER: 1.2  DAT: 09-03-75  CMP: ALL                       20740
C     PGM: PASS TWO OF THE M6800 CROSS-ASSEMBLER                           20745
C                                                                          20750
C     SYS: M68SAM                                                          20755
C                                                                          20760
C     ENT: N/A                                                             20765
C     RTN: N/A                                                             20770
C                                                                          20775
C     FNC: PASS TWO, RE-READ THE SOURCE FILE AND OUTPUT LISTING            20780
C          AND OBJECT FILE (PAPER TAPE)                                    20785
C                                                                          20790
C     REV: 1.2 - CALL TO MPARSI ADDED                                      20795
C*                                                                         20800
      COMMON ICOMON(2),LUSI,LUOT,LLSPSP,IBPWD(20),ISIBUF(80)               20805
      COMMON NB(122),LABEL(4),LABESW,IOPCOD(4),NPNAM(98)                   20810
      COMMON ISIMBF(43),IC,NC,ICSW,IPASS,IOPCLS,IOPIXB,IOPBIN              20815
      COMMON IOPAN1,IPCT,ITERR,NRTSW,NSSCOL,NSOSCH,NAMSW,LSCM              20820
      COMMON LIMA(17),LSP,LASK(8),LA,LB                                    20825
      EQUIVALENCE (ISILN,ISIMBF(2))                                        20830
      COMMON /A/ NOPCD(246),IOPBC2(51),IOPBC3(16)                          20835
      COMMON /A/ IOPBC4(116),IOPBC5(22)                                    20840
      IE=204                                                               20845
      IPCT=0                                                               20850
      JRDNO=0                                                              20855
      JRLNSW=2                                                             20860
C***  REWIND THE SOURCE INPUT FILE                                         20865
      CALL MPARSI (3)                                                      20870
100   DO 105 I=1,80                                                        20875
      ISIBUF(I)=LLSPSP                                                     20880
105   CONTINUE                                                             20885
C                                                                          20890
C***  READ IN THE NEXT SOURCE RECORD                                       20895
C                                                                          20900
      CALL MPARSI (4)                                                      20905
      JRDNO=JRDNO+1                                                        20910
      ISILN=JRDNO                                                          20915
      CALL MPUPIB (ISIBUF,JRLNSW)                                          20920
      LEVEL=1                                                              20925
      NRTSW=1                                                              20930
      NSOSCH=LSP                                                           20935
      IOPAN1=0                                                             20940
      LABEL(1)=0                                                           20945
      GO TO 120                                                            20950
C***  GET THE NEXT CHAR                                                    20955
110   CALL MPUGNC (NC)                                                     20960
120   GO TO (130,220,240,280),LEVEL                                        20965
130   GO TO (210,160,140,200,180,150),ICSW                                 20970
C***  ERROR '202' LABEL OR OPCODE MUST START WITH A ALPHA CHAR             20975
140   IE=IE-1                                                              20980
C***  ERROR '203' BLANK RECORD OR THE RECORD ONLY CONTAINS A LABEL         20985
150   IE=IE-1                                                              20990
C***  ERROR '204' SYNTAX ERROR                                             20995
160   CALL MPAERR (IE)                                                     21000
      IE=204                                                               21005
170   IOPBIN=0                                                             21010
      IOPAN1=0                                                             21015
      NRTSW=1                                                              21020
      CALL MPAPRL(311)                                                     21025
      IPCT=IPCT+3                                                          21030
      GO TO 100                                                            21035
180   IF(NC.NE.LSCM) GO TO 160                                             21040
C***  RECORD IS A COMMENT RECORD (SKIPIT)                                  21045
190   CALL MPAPRL(33)                                                      21050
      GO TO 100                                                            21055
C***  GET THE STATEMENT'S LABEL                                            21060
200   CALL MPUBN8 (LABEL)                                                  21065
      LABEL(4)=0                                                           21070
210   LEVEL=2                                                              21075
      GO TO 110                                                            21080
C***  LEVEL 2 SCAN FOR START OF OPCODE FIELD                               21085
220   GO TO (110,140,140,230,140,150),ICSW                                 21090
C***  GET THE OPCODE                                                       21095
230   CALL MPUBN8 (IOPCOD)                                                 21100
      IF(ICSW.NE.1) GO TO 160                                              21105
      CALL MPAFOP                                                          21110
      IF(IOPCLS.EQ.2) GO TO 300                                            21115
      LEVEL=3                                                              21120
C***  LEVEL 3 SCAN FOR 'A ' OR 'B '                                        21125
240   GO TO (110,290,290,250,280,290),ICSW                                 21130
250   IF(NC.NE.LA.AND.NC.NE.LB) GO TO 290                                  21135
      JC=NC                                                                21140
      CALL MPUGNC(NC)                                                      21145
      IF(ICSW.EQ.1) GO TO 260                                              21150
      IC=IC-2                                                              21155
      CALL MPUGNC(NC)                                                      21160
      GO TO 270                                                            21165
C***  OPCODE IS 'CCC X ' WHERE: X= A OR B                                  21170
260   IOPCOD(2)=IOPCOD(2)-LSP+JC                                           21175
      CALL MPAFOP                                                          21180
      IF(IOPCLS.EQ.2) GO TO 300                                            21185
270   LEVEL=4                                                              21190
C***  LEVEL 4 SCAN TO THE START OF THE OPERAND                             21195
280   GO TO (110,290,290,290,290,290),ICSW                                 21200
290   NRTSW=3                                                              21205
300   NSSCOL=IC                                                            21210
      IF(LABEL(1).EQ.0.OR.IOPCLS.EQ.1) GO TO 310                           21215
      CALL MPAFSY (LABEL,J,I)                                              21220
      IF(LABEL(4).EQ.IPCT) GO TO 310                                       21225
C***  ERROR '220' PHASING ERROR                                            21230
      CALL MPAERR (220)                                                    21235
      IPCT=LABEL(4)                                                        21240
310   GO TO (320,370,380,390,480),IOPCLS                                   21245
C                                                                          21250
C***  DIRECTIVE OPCODES                                                    21255
C                                                                          21260
C***  ASSEMBLER CLASS 1                                                    21265
320   IF(IOPIXB.GT.4) GO TO 360                                            21270
      GO TO (330,190,340,340),IOPIXB                                       21275
C***  ERROR '207' UNDEFINED OPCODE                                         21280
330   CALL MPAERR (207)                                                    21285
      GO TO 170                                                            21290
C***  'END' OPCODE                                                         21295
340   CALL MPAPRL(33)                                                      21300
C***  OUTPUT THE LAST P.T. RECORD                                          21305
      CALL MPUPTS (3,0,0)                                                  21310
      RETURN                                                               21315
C***  GO TO THE DIRECTIVE OPCODE PROCESSOR                                 21320
360   CALL MPAPSC                                                          21325
      GO TO 100                                                            21330
C                                                                          21335
C***  1 BYTE INSTURCTIONS                                                  21340
C                                                                          21345
C***  ASSEMBLER CLASS 2                                                    21350
370   IOPBIN=IOPBC2(IOPIXB)                                                21355
      NRTSW=2                                                              21360
      CALL MPAPRL(295)                                                     21365
      IPCT=IPCT+1                                                          21370
      GO TO 100                                                            21375
C                                                                          21380
C***  2 BYTE RELATIVE INSTRUCTIONS                                         21385
C                                                                          21390
C***  ASSEMBLER CLASS 3                                                    21395
380   IOPBIN=IOPBC3(IOPIXB)                                                21400
      CALL MPAOPR (IOPAN1,JOPRSW)                                          21405
C***  ERROR '208' RELATIVE BRANCH OUT OF RANGE                             21410
      IF(IOPAN1.LT.IPCT-125.OR.IOPAN1.GT.IPCT+129) CALL MPAERR (208)       21415
      CALL MPUADR (IOPAN1,2,IPCT,JSW)                                      21420
      CALL MPUADR (IOPAN1,2,2,JSW)                                         21425
      CALL MPAPRL(303)                                                     21430
      IPCT=IPCT+2                                                          21435
      GO TO 100                                                            21440
C                                                                          21445
C***  2 OR 3 BYTE INSTRUCTIONS                                             21450
C                                                                          21455
C***  ASSEMBLER CLASS 4                                                    21460
390   CALL MPAGAM (IAMOD,JOPRSW)                                           21465
      IF(JOPRSW.GT.4) GO TO 160                                            21470
      IOPIXB=IOPIXB*4-4+IAMOD                                              21475
      IOPBIN=IOPBC4(IOPIXB)                                                21480
      IF(IOPBIN.GE.0) GO TO (410,420,450,460),IAMOD                        21485
C***  ERROR '209' ILLEGAL ADDRESS MODE                                     21490
400   CALL MPAERR (209)                                                    21495
      IOPBIN=0                                                             21500
      GO TO 470                                                            21505
C***  DIRECT ADDRESS MODE                                                  21510
C***  CHANGE MODE FOR 'JMP' AND 'JSR' TO EXTENDED                          21515
410   IF(IOPIXB.EQ.21.OR.IOPIXB.EQ.25) GO TO 460                           21520
      GO TO 430                                                            21525
C***  INDEXED ADDRESS MODE                                                 21530
420   CONTINUE                                                             21535
430   IF(JOPRSW.EQ.1) GO TO 440                                            21540
C***  ERROR '210' BYTE OVERFLOW (> 256)                                    21545
      CALL MPAERR (210)                                                    21550
      IOPAN1=255                                                           21555
440   CALL MPAPRL(303)                                                     21560
      IPCT=IPCT+2                                                          21565
      GO TO 100                                                            21570
C***  IMMEDIATE ADDRESS MODE                                               21575
450   IF(IOPIXB.LT.13) GO TO 470                                           21580
      GO TO 430                                                            21585
C***  EXTENDED ADDRESS MODE                                                21590
460   CONTINUE                                                             21595
470   CONTINUE                                                             21600
      CALL MPAPRL(311)                                                     21605
      IPCT=IPCT+3                                                          21610
      GO TO 100                                                            21615
C                                                                          21620
C***  2 OR 3 BYTE INSTRUCTIONS (INDEXED OR EXTENDED MODE ONLY)             21625
C                                                                          21630
C***  ASSEMBLER CLASS 5                                                    21635
480   IOPIXB=IOPIXB*2-1                                                    21640
      CALL MPAGAM (IAMOD,JOPRSW)                                           21645
      IF(JOPRSW.GT.4) GO TO 160                                            21650
      GO TO (500,490,400,500),IAMOD                                        21655
490   IOPBIN=IOPBC5(IOPIXB)                                                21660
      GO TO 430                                                            21665
500   IOPBIN=IOPBC5(IOPIXB+1)                                              21670
      GO TO 470                                                            21675
      END                                                                  21680
      SUBROUTINE MPAM3                                                     21685
C+    NAM: MPAM3   VER: 1.0  DAT: 08-27-75  CMP: ALL                       21690
C     PGM: PASS THREE OF THE MPU CROSS-ASSEMBLER                           21695
C                                                                          21700
C     SYS: M68SAM                                                          21705
C                                                                          21710
C     ENT: N/A                                                             21715
C     RTN: N/A                                                             21720
C                                                                          21725
C     FNC: PRINT THE SYMBOL TABLE                                          21730
C*                                                                         21735
      COMMON ICOMON,LULT,LUSI(20),KCFF,KC7F7F,KC80(81),NB(121)             21740
      COMMON INX,LABEL(14),IPGLOL,IPGLEN,NPLCT,ICSHF,IOTBUF(84)            21745
      COMMON NAM(5),ISIMBF(52),ITERR,NRTSW(10),NOPT(2),NOPC(5)             21750
      COMMON IXBASE,LSPSP(4),LSP,LASK(20),NOSYM,ISYM(813)                  21755
      EQUIVALENCE (NOPBAS,NOPT(1)),(NOPLI,NOPT(2))                         21760
      INTEGER JLDF(3)                                                      21765
      DATA JLDF(1)/6/,JLDF(2)/5/,JLDF(3)/4/                                21770
      IF(NOPLI.EQ.1.OR.NOSYM.LT.4) GO TO 170                               21775
      JLF=JLDF(IXBASE)                                                     21780
      NL=IPGLOL/(JLF+9)                                                    21785
      IF(NPLCT.LT.10) GO TO 100                                            21790
C<UT>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C<UT>  IF(NOSYM/NL+NPLCT+10.GT.IPGLEN) CALL MPAPAG(0)                
C<UT>  FORCE SYMBOL TABLE ON SEPARATE PAGE
	CALL	MPAPAG(0)
C<UT><<<<<<<<UT<<<<<<<<<<<<UT<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
100   WRITE (LULT,110)                                                     21800
      WRITE (LULT,110)                                                     21805
110   FORMAT (1H )                                                         21810
      WRITE (LULT,120)                                                     21815
120   FORMAT (13H SYMBOL TABLE)                                            21820
      WRITE (LULT,110)                                                     21825
      WRITE (LULT,110)                                                     21830
      CALL MPAPAG (5)                                                      21835
      N=3                                                                  21840
130   L=0                                                                  21845
      INX=0                                                                21850
140   N=N+1                                                                21855
      J=N*4-3                                                              21860
      NAM(1)=MPUAND(KC7F7F,ISYM(J))                                        21865
      NAM(2)=ISYM(J+1)                                                     21870
      NAM(3)=ISYM(J+2)                                                     21875
      NAM(4)=ISYM(J+3)                                                     21880
      CALL MPUSNC(LSP)                                                     21885
      DO 150I=1,3                                                          21890
      J=NAM(I)/ICSHF                                                       21895
      CALL MPUSNC(J)                                                       21900
      J=MPUAND(KCFF,NAM(I))                                                21905
      CALL MPUSNC(J)                                                       21910
150   CONTINUE                                                             21915
      CALL MPUSNC(LSP)                                                     21920
      CALL MPUCNA (NAM(4),NOPBAS,JLF)                                      21925
      CALL MPUSNC(LSP)                                                     21930
      L=L+1                                                                21935
      IF(N.GE.NOSYM) GO TO 160                                             21940
      IF(L.LT.NL) GO TO 140                                                21945
160   CALL MPAPA1 (2,NB,INX)                                               21950
C<UT>>>>>>>>>>>>>>>>>>>>>>>>>>UT>>>>>>>UT>>>>>>>>>>>>>>>>>>
C<UT> FIX BLANK PAGE AT END PROBLEM
C<UT>  CALL MPAPAG (1)                                    
C<UT>      IF(N.LT.NOSYM) GO TO 130                  
	IF (N.GE.NOSYM) GO TO 170
	CALL MPAPAG(1)
	GO TO 130
C<UT><<<<<<UT<<<<<<<<<<UT<<<<<<<<<<UT<<<<<<<<UT<<<<<<<<<<<<<<<<
170   IF(ITERR.EQ.0) GO TO 190                                             21965
      WRITE (LULT,110)                                                     21970
      WRITE (LULT,110)                                                     21975
      WRITE (LULT,180) ITERR                                               21980
180   FORMAT (13H TOTAL ERRORS,I4)                                         21985
190   RETURN                                                               21990
      END                                                                  21995
��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������