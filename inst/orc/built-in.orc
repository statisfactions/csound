sr = 20000
kr = 2000
nchnls = 2
0dbfs = 1

gitab ftgen 1, 0, 2048, 10, 1

instr 1
/* Basic FM synthesis instrument */
  idur    = p3
  iamp    = p4
  ipan	  = p5
  iattkp  = p6
  idecayp = p7
  icps    = p8
  imod    = p9
  indx    = p10

  kenv linen iamp, iattkp*idur, idur, (1-idecayp)*idur
  asig foscili iamp*kenv, icps, 1, imod, indx, gitab
  outs asig*ipan, asig*(1-ipan)

endin

instr 2
/* Basic band-pass subtractive synthesis */
  idur	   = p3
  iamp	   = p4
  ipan	   = p5
  iattkp   = p6
  idecayp  = p7
  kcntr	   = p8
  kbw	   = p9

  kenv linen iamp, iattkp*idur, idur, (1-idecayp)*idur
  anoise rand kenv
  asig reson anoise, kcntr, kbw
  outs asig*ipan, asig*(1-ipan)

endin