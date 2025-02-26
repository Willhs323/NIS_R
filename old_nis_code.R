



coretest <- read_fwf(
  file = "NIS_2021 (4)/ASCII files/NIS_2021_Core.ASC",
  col_positions = fwf_cols(3,2,2,2,2,11,2,2,3,2,3,2,2,3,2,5,3,3,7,7,
                           7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
                           7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,2,2,
                           2,2,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
                           7,7,7,7,7,7,7,3,10,5,2,2,4,2,2,3,3,3,3,3,
                           3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                           3,2,10,2,2,4,2,7),
  na = missing_values,
  n_max = 100)
coretest

colnames(coretest) <- c("Age", "Age_neonate", "Amonth", "Aweekend", "Died", "DISCWT",
                        "DISPUNIFORM", "DQTR", "DRG", "DRGVER", "DRG_NoPOA", "Elective",
                        "Female", "HCUP_ED", "HOSP_division", "Hisp_NIS", "I10_birth", "I10_delivery",
                        "I10_DX1", "I10_DX2", "I10_DX4", "I10_DX5", "I10_DX6", "I10_DX7", "I10_DX8", "I10_DX9", "I10_DX10",
                        "I10_DX11", "I10_DX12", "I10_DX13", "I10_DX14", "I10_DX15", "I10_DX16", "I10_DX17", "I10_DX18", "I10_DX19", "I10_DX20",
                        "I10_DX21", "I10_DX22", "I10_DX23", "I10_DX24", "I10_DX25", "I10_DX26", "I10_DX27", "I10_DX28", "I10_DX29", "I10_DX30",
                        "I10_DX31", "I10_DX32","I10_DX33", "I10_DX34", "I10_DX35", "I10_DX36", "I10_DX37", "I10_DX38", "I10_DX39", "I10_DX40",
                        "I10_Injury", "I10_multiinjury", "I10NDX", "I10_NPR",
                        "I10_PR1", "I10_PR2", "I10_PR4", "I10_PR5", "I10_PR6", "I10_PR7", "I10_PR8", "I10_PR9", "I10_PR10",
                        "I10_PR11", "I10_PR12", "I10_PR14", "I10_PR15", "I10_PR16", "I10_PR17", "I10_PR18", "I10_PR19", "I10_PR20",
                        "I10_PR21", "I10_PR22", "I10_PR24", "I10_PR25",
                        "I10_serviceline", "Key_NIS", "LOS", "MDC", "MDC_NoPOA", "NIS_stratum", "PAY1", "Pclass_ORPROC", "PL_NCHS",
                        "PRDAY1","PRDAY2","PRDAY3","PRDAY4","PRDAY5","PRDAY6","PRDAY7","PRDAY8","PRDAY9","PRDAY10","PRDAY11","PRDAY12","PRDAY13",
                        "PRDAY14","PRDAY15","PRDAY16","PRDAY17","PRDAY18","PRDAY19","PRDAY20","PRDAY21","PRDAY22","PRDAY23","PRDAY24","PRDAY25",
                        "Race", "TOTCHG", "TRAN_IN", "TRAN_OUT", "Year", "ZIPINC_QRTL"
)

coretest

dim(coretest)

View(coretest)


# remove neonate, Amonth, Aweekend, 
coretesticd <- coretest[,c(15:124)]
coretesticd

View(coretesticd)
str(coretest)


colposit <- c(3,2,2,2,2,11,2,2,3,2,3,2,2,3,2,5,3,3,7,7,
              7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
              7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,2,2,
              2,2,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
              7,7,7,7,7,7,7,3,10,5,2,2,4,2,2,3,3,3,3,3,
              3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
              3,2,10,2,2,4,2)

sum(colposit)
length((colposit))


# .txt file

1 AGE                              1    3
2 AGE_NEONATE                      4    5
3 AMONTH                           6    7
4 AWEEKEND                         8    9
5 DIED                            10   11
6 DISCWT                          12   22
7 DISPUNIFORM                     23   24
8 DQTR                            25   26
9 DRG                             27   29
10 DRGVER                          30   31
11 DRG_NoPOA                       32   34
12 ELECTIVE                        35   36
13 FEMALE                          37   38
14 HCUP_ED                         39   41
15 HOSP_DIVISION                   42   43
16 HOSP_NIS                        44   48
17 I10_BIRTH                       49   51
18 I10_DELIVERY                    52   54
19 I10_DX1                         55   61
20 I10_DX2                         62   68
21 I10_DX3                         69   75
22 I10_DX4                         76   82
23 I10_DX5                         83   89
24 I10_DX6                         90   96
25 I10_DX7                         97  103
26 I10_DX8                        104  110
27 I10_DX9                        111  117
28 I10_DX10                       118  124
29 I10_DX11                       125  131
30 I10_DX12                       132  138
31 I10_DX13                       139  145
32 I10_DX14                       146  152
33 I10_DX15                       153  159
34 I10_DX16                       160  166
35 I10_DX17                       167  173
36 I10_DX18                       174  180
37 I10_DX19                       181  187
38 I10_DX20                       188  194
39 I10_DX21                       195  201
40 I10_DX22                       202  208
41 I10_DX23                       209  215
42 I10_DX24                       216  222
43 I10_DX25                       223  229
44 I10_DX26                       230  236
45 I10_DX27                       237  243
46 I10_DX28                       244  250
47 I10_DX29                       251  257
48 I10_DX30                       258  264
49 I10_DX31                       265  271
50 I10_DX32                       272  278
51 I10_DX33                       279  285
52 I10_DX34                       286  292
53 I10_DX35                       293  299
54 I10_DX36                       300  306
55 I10_DX37                       307  313
56 I10_DX38                       314  320
57 I10_DX39                       321  327
58 I10_DX40                       328  334
59 I10_INJURY                     335  336
60 I10_MULTINJURY                 337  338
61 I10_NDX                        339  340
62 I10_NPR                        341  342
63 I10_PR1                        343  349
64 I10_PR2                        350  356
65 I10_PR3                        357  363
67 I10_PR5                        371  377
68 I10_PR6                        378  384
69 I10_PR7                        385  391
70 I10_PR8                        392  398
71 I10_PR9                        399  405
72 I10_PR10                       406  412
73 I10_PR11                       413  419
74 I10_PR12                       420  426
75 I10_PR13                       427  433
76 I10_PR14                       434  440
77 I10_PR15                       441  447
78 I10_PR16                       448  454
79 I10_PR17                       455  461
80 I10_PR18                       462  468
81 I10_PR19                       469  475
82 I10_PR20                       476  482
83 I10_PR21                       483  489
84 I10_PR22                       490  496
85 I10_PR23                       497  503
86 I10_PR24                       504  510
87 I10_PR25                       511  517
88 I10_SERVICELINE                518  520
89 KEY_NIS                        521  530
90 LOS                            531  535
91 MDC                            536  537
92 MDC_NoPOA                      538  539
93 NIS_STRATUM                    540  543
94 PAY1                           544  545
95 PCLASS_ORPROC                  546  547
96 PL_NCHS                        548  550
97 PRDAY1                         551  553
98 PRDAY2                         554  556
99 PRDAY3                         557  559
100 PRDAY4                         560  562
101 PRDAY5                         563  565
102 PRDAY6                         566  568
103 PRDAY7                         569  571
104 PRDAY8                         572  574
105 PRDAY9                         575  577
106 PRDAY10                        578  580
107 PRDAY11                        581  583
108 PRDAY12                        584  586
109 PRDAY13                        587  589
110 PRDAY14                        590  592
111 PRDAY15                        593  595
112 PRDAY16                        596  598
113 PRDAY17                        599  601
114 PRDAY18                        602  604
115 PRDAY19                        605  607
116 PRDAY20                        608  610
117 PRDAY21                        611  613
118 PRDAY22                        614  616
119 PRDAY23                        617  619
120 PRDAY24                        620  622
121 PRDAY25                        623  625
122 RACE                           626  627
123 TOTCHG                         628  637
124 TRAN_IN                        638  639
125 TRAN_OUT                       640  641
126 YEAR                           642  645
127 ZIPINC_QRTL                    646  647


