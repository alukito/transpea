module LuXiaoYu where
import Euterpea

rhythm1 = timesM 2 (line [enr, c 4 sn, d 4 sn, e 4 sn, d 4 sn, c 4 sn, e 4 sn]) 
rhythm2 = timesM 2 (line [enr, b 3 sn, c 4 sn, d 4 sn, c 4 sn, b 3 sn, d 4 sn])

rhythm = rhythm1 :+: rhythm2 :+: timesM 2 rhythm1

bass = timesM 2 (line [c 3 sn, g 3 sn, enr, qnr]) :+:
       timesM 2 (line [e 3 sn, g 3 sn, enr, qnr]) :+:
       timesM 2 (line [f 3 sn, a 3 sn, enr, qnr]) :+:
       line [f 3 sn, af 3 sn, enr, qnr, f 3 sn, a 3 sn, enr, qnr]

       
bass13 = timesM 2 (line [e 3 sn, g 3 sn, enr, qnr]) :+:
         line [ef 3 sn, fs 3 sn, enr, qnr, ds 3 sn, fs 3 sn, enr, qnr] :+:
         timesM 2 (line [d 3 sn, f 3 sn, enr, qnr])         
       
rhythm13 = timesM 2 (line [enr, b 3 sn, c 4 sn, g 4 sn, c 4 sn, b 3 sn, g 4 sn]) :+:
           timesM 2 (line [enr, a 3 sn, c 4 sn, fs 4 sn, c 4 sn, a 3 sn, fs 4 sn]) :+:
           timesM 2 (line [enr, a 3 sn, c 4 sn, (f 4 sn :=: a 4 sn), c 4 sn, a 3 sn, (f 4 sn :=: a 4 sn)])
           
melody = timesM 3 wnr :+: hnr :+: qnr :+: f 5 qn :+: e 5 dhn :+: e 5 qn :+: d 5 dhn :+: d 5 qn :+:
         c 5 dhn :+: c 5 qn :+: g 5 qn :+: f 5 qn :+: c 5 qn :+: d 5 qn :+: e 5 dhn :+: e 5 qn :+: 
         b 5 dhn :+: c 6 qn :+: a 5 dhn :+: a 5 qn :+: e 6 qn :+: f 6 qn :+: c 6 qn :+: d 6 qn :+:
         b 5 dhn :+: g 5 qn :+: a 5 dhn :+: a 5 qn :+: e 6 hn :+: c 6 hn

realBass = timesM 3 bass :+: bass13
realRhythm = timesM 3 rhythm :+: rhythm13

music = instrument AcousticGrandPiano $ tempo 0.6 (realBass :=: realRhythm :=: melody)