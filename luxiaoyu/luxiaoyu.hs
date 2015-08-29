module LuXiaoYu where
import Euterpea

addDur :: Dur -> [Dur -> Music a] -> Music a
addDur d ns = line $ map (\n -> n d) ns

bass1 = timesM 2 (line [c 3 sn, g 3 sn, enr, qnr])
bass2 = timesM 2 (line [e 3 sn, g 3 sn, enr, qnr])
bass3 = timesM 2 (line [f 3 sn, a 3 sn, enr, qnr])
bass4 = timesM 2 (line [f 3 sn, af 3 sn, enr, qnr])

treble1 = timesM 2 (enr :+: addDur sn [c 4, d 4, e 4, d 4, c 4, e 4])
treble2 = timesM 2 (enr :+: addDur sn [b 3, c 4, d 4, c 4, b 3, d 4])

riff1_4 = line [bass1, bass2, bass3, bass4] :=:
          treble1 :+: treble2 :+: timesM 2 treble1

treble13_16 = timesM 2 (line [enr, addDur sn [b 3, c 4, g 4, c 4, b 3, g 4] ]) :+:
              timesM 2 (line [enr, addDur sn [a 3, c 4, fs 4, c 4, a 3, fs 4] ]) :+:
              timesM 2 (line [enr, addDur sn [a 3, c 4], f 4 sn :=: a 4 sn, addDur sn [c 4, a 3], f 4 sn :=: a 4 sn]) :+:
              timesM 2 (line [enr, addDur sn [c 4, f 4], a 4 sn :=: c 5 sn, addDur sn [f 4, c 4], a 4 sn :=: c 5 sn])

bass13_16 = bass2 :+:
            line [ef 3 sn, fs 3 sn, enr, qnr, ds 3 sn, fs 3 sn, enr, qnr] :+:
            timesM 2 (line [d 3 sn, f 3 sn, enr, qnr]) :+:
            timesM 2 (line [g 2 sn, f 3 sn, enr, qnr])

bass17 = c 3 wn :=: c 2 wn
bass17_20 = line [bass17, a 2 wn :=: a 1 wn] :+:
            line [fs 2 wn :=: fs 1 wn, f 2 hn :=: f 1 hn, g 2 hn :=: g 1 hn]

treble17 = let treble17b = addDur sn [b 4, e 4, g 4, b 4]
           in  addDur sn [c 5, e 4, g 4, c 5] :+:
               treble17b :+:
               addDur sn [a 4, d 4, f 4, a 4] :+:
               treble17b

treble19 = let treble19b = addDur sn [c 5, fs 4, a 4, c 5]
           in  addDur sn [d 5, fs 4, a 4, d 5] :+:
               treble19b :+:
               addDur sn [b 4, fs 4, a 4, b 4] :+:
               treble19b

treble20 = addDur sn [d 5, f 4, gs 4, d 5] :+:
           addDur sn [c 5, f 4, af 4, c 5] :+:
           addDur sn [b 4, c 4, f 4, b 4] :+:
           addDur sn [c 5, d 4, f 4, c 5]

riff17_20 = bass17_20 :=:
            line [timesM 2 treble17, treble19, treble20]

riff29 = bass17 :=: addDur sfn [c 4, d 4] :+: e 4 (hn + dqn)

riff = timesM 3 riff1_4 :+:
       bass13_16 :=: treble13_16 :+:
       timesM 2 riff17_20 :+:
       riff1_4 :+:
       riff29

melody17_20 = let melody17 = addDur qn [d 6, c 6, b 5, c 6]
              in  timesM 2 melody17 :+:
                  line [addDur qn [e 6, d 6, c 6, d 6, e 6, d 6, f 6, e 6]]

melody = line [timesM 3 wnr, hnr, qnr] :+:
         line [f 5 qn, e 5 dhn, e 5 qn, d 5 dhn, d 5 qn] :+:
         line [c 5 dhn, addDur qn [c 5, g 5 ,f 5, c 5, d 5], e 5 dhn, e 5 qn] :+:
         line [b 5 dhn, c 6 qn, a 5 dhn, addDur qn [a 5, e 6, f 6, c 6, d 6]] :+:
         line [b 5 dhn, g 5 qn, a 5 dhn, a 5 qn, addDur hn [e 6, c 6]] :+:
         line [addDur hn [e 6, c 6], timesM 2 melody17_20] :+:
         line [c 6 dhn, e 6 qn, d 6 dhn, d 6 qn] :+:
         line [c 6 dhn, c 6 qn, addDur qn [g 6, f 6, c 6, d 6]] :+:
         c 6 wn

music = instrument (fromGM 1) $ tempo 0.6 (riff :=: melody)
