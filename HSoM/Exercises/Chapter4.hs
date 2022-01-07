module HSoM.Exercises.Chapter4 where

import Euterpea

-- Twinkle Twinkle Little Star

pcToQN :: PitchClass -> Music Pitch 
pcToQN pc = note qn (pc, 4)

twinkle =
    let m1 = line (map pcToQN [C,C,G,G,A,A]) :+: g 4 hn
        m2 = line (map pcToQN [F,F,E,E,D,D]) :+: c 4 hn 
        m3 = line (map pcToQN [G,G,F,F,E,E]) :+: d 4 hn
    in line [m1, m2, m3, m3, m1, m2]

-- Chick Corea's "Children's Song No. 6"

-- Helper functions

-- addDur' adds the duration to a list of notes, but doesn't turn it into a line,
-- so can be used in other maps over notes
addDur' :: Dur -> [Dur -> Music a] -> [Music a]
addDur' d ns = let f n = n d
               in map f ns

-- Like the addDur in the book, but uses addDur'
addDur :: Dur -> [Dur -> Music a] -> Music a
addDur d ns = line (addDur' d ns)

graceNote :: Int -> Music Pitch -> Music Pitch
graceNote n (Prim (Note d p)) =
    note (d/8) (trans n p) :+: note (7 * d/8) p
graceNote _  _ = error "Can only add a grace note to a note"

-- Base Line
b1 = addDur dqn [b 3, fs 4, g 4, fs 4]
b2 = addDur dqn [b 3, es 4, fs 4, es 4]
b3 = addDur dqn [as 3, fs 4, g 4, fs 4]

baseLine = times 3 b1 :+: times 2 b2 :+: times 4 b2 :+: times 5 b1

-- Main Voice

v1 = v1a :+: graceNote (-1) (d 5 qn) :+: v1b        -- bars 1-2
v1a = addDur en [a 5, e 5, d 5, fs 5, cs 5, b 4, e 5, b 4]
v1b = addDur en [cs 5, b 4]

v2 = v2a :+: v2b :+: v2c :+: v2d :+: v2e :+: v2f :+: v2g
v2a = line [cs 5 (dhn + dhn), d 5 dhn, f 5 hn, gs 5 qn, fs 5 (hn + en), g 5 en]     -- bars 7-11
v2b = addDur en [fs 5, e 5, cs 5, as 4] :+: a 4 dqn :+: 
      addDur en [as 4, cs 5, fs 5, e 5, fs 5]                                       -- bars 12-13
v2c = line [g 5 en, as 5 en, cs 6 (hn + en), d 6 en, cs 6 en] :+: e 5 en :+: enr :+:
      line [as 5 en, a 5 en, g 5 en, d 5 qn, c 5 en, cs 5 en]                       -- bars 14-16
v2d = addDur en [fs 5, cs 5, e 5, cs 5, a 4, as 4, d 5, e 5, fs 5]                  -- bars 17-18.5
v2e = line [graceNote 2 (e 5 qn), d 5 en, graceNote 2 (d 5 qn), cs 5 en,
            graceNote 1 (cs 5 qn), b 4 (en+hn), cs 5 en, b 4 en]                    -- bars 18.5-20
v2f = line [fs 5 en, a 5 en, b 5 (hn+qn), a 5 en, fs 5 en, e 5 qn,
            d 5 en, fs 5 en, e 5 hn, d 5 hn, fs 5 qn]                               -- bars 21-23
v2g = tempo (3/2) (line [cs 5 en, d 5 en, cs 5 en]) :+: b 4 (3*dhn+hn)              -- bars 24-28

mainVoice = times 3 v1 :+: v2

childSong6 :: Music Pitch 
childSong6 = let t = (dhn/qn) * (69/120)
             in instrument RhodesPiano (tempo t (baseLine :=: mainVoice))


-- Execrcise 4.1

-- Pachabel

-- To simplify encoding several measures of chords of the same duration
-- The notes of the chords are a list of Dur -> Music a. We add the duration
-- with addDur' and use chord to turn the notes into chords
addDurChord :: Dur -> [[Dur -> Music a]] -> Music a
addDurChord d ns = line $ map (chord . addDur' d) ns

-- The ri are right hand phrases and li are left hand phrases

r1 = qnr :+: addDur qn [fs 4, a 4, d 5] :+: qnr :+: addDur qn [e 4, a 4, cs 5] :+: 
     qnr :+: addDur qn [d 4, fs 4, b 4] :+: qnr :+: addDur qn [cs 4, fs 4, a 4] :+:
     qnr :+: addDur qn [d 4, g 4, b 4] :+: qnr :+: addDur qn [ d 4, fs 4, a 4] :+:
     qnr :+: addDur qn [d 4, g 4, b 4] :+: qnr :+: addDur qn [e 4, a 4, cs 5]

r2 = addDur wn [d 5, cs 5, b 4, a 4, g 4, a 4, b 4, cs 5]

r3 = addDurChord wn [[d 5, fs 5], [cs 5, e 5], [b 4, d 5], [a 4, cs 5], [g 4, b 4], 
                     [fs 4, a 4], [g 4, b 4], [a 4, cs 5]]

r4 = line $ [ fs 5 qn, d 5 en, e 5 en, fs 5 qn, e 5 en, d 5 en,
              e 5 en, cs 5 en, d 5 en, e 5 en, fs 5 en, e 5 en, d 5 en, cs 5 en,
              d 5 qn, b 4 en, cs 5 en, d 5 qn, fs 4 en, g 4 en,
              a 4 en, b 4 en, a 4 en, g 4 en, a 4 en, d 5 en, cs 5 en, d 5 en,
              b 4 qn, d 5 en, cs 5 en, b 4 qn, a 4 en, g 4 en,
              a 4 en, g 4 en, fs 4 en, g 4 en, a 4 en, b 4 en, cs 5 en, d 5 en,
              b 4 qn, d 5 en, cs 5 en, d 5 qn, cs 5 en, d 5 en,
              cs 5 en, a 4 en, b 4 en, cs 5 en, d 5 en, e 5 en, fs 5 en, g 5 en]

r5 = chord [fs 5 dqn, a 5 dqn]
     :+: line [ a 5 qn, a 5 qn, b 5 qn, a 5 qn, g 5 qn,
                fs 5 dqn, fs 5 qn, fs 5 qn, g 5 qn, fs 5 qn, e 5 qn,
                d 5 qn, cs 5 qn, b 4 qn, cs 5 qn,
                d 5 hn, a 4 hn, d 5 qn, c 5 qn, b 4 hn, cs 5 wn
                ]

r6 = let
        chords = [ [d 5, fs 5]
                 , [cs 5, e 5]
                 , [b 4, d 5]
                 , [a 4, cs 5]
                 , [g 4, b 4]
                 , [fs 4, a 4]
                 , [g 4, b 4]
                 , [a 4, cs 5]
                 ]
     in line $ map (\c -> hnr :+: (chord . addDur' hn) c) chords

r7 = chord [ fs 4 wn, d 5 wn]

l1 = addDur wn [d 4, a 3, b 3, fs 3, g 3, d 3, g 3, a 3]

l2 = addDur qn  [ d 3, a 3, d 4, fs 4
                , a 2, e 3, a 3, cs 4 
                , b 2, fs 3, b 3, d 4
                , fs 2, cs 3, fs 3, a 3
                , g 2, d 3, g 3, b 3
                , d 2, a 2, d 3, fs 3
                , g 2, d 3, g 3, b 3
                , a 2, e 3, a 3, cs 4
                ]

-- Measures 25 - 32 (l3) and 41 - 48 (l3')
-- The first measure is transposed an octave lower in the second phrase
l3a = line [d 3 qn, a 3 qn, d 4 hn]

l3b = line [ a 2 qn, e 3 qn, a 3 hn
           , b 2 qn, fs 3 qn, b 3 hn
           , fs 2 qn, cs 3 qn, fs 3 hn
           , g 2 qn, d 3 qn, g 3 hn
           , d 2 qn, a 2 qn, d 3 hn
           , g 2 qn, d 3 qn, g 3 hn 
           , a 2 qn, e 3 qn, a 3 hn
           ]

l3 = l3a :+: l3b

l3' = transpose (-12) l3a :+: l3b

l4 = addDur qn [ d 3, a 3, d 4, a 3
               , a 2, e 3, a 3, e 3
               , b 2, fs 3, b 3, fs 3
               , fs 2, cs 3, fs 3, cs 3
               , g 2, d 3, g 3, d 3
               , d 3, a 3, d 4, a 3
               , g 2, d 3, g 3, d 3
               ]
        :+: 
            line [a 2 qn, e 3 qn, a 3 hn]

l5 = chord [a 2 wn, d 2 wn]

rh = r1 :+: r2 :+: r3 :+: r4 :+: r5 :+: r6 :+: r7
lh = l1 :+: times 2 l2 :+: l3 :+: l4 :+: l3' :+: l5

pachabel :: InstrumentName -> InstrumentName -> Music Pitch
pachabel upper lower = 
    instrument upper rh :=: instrument lower lh


-- Prefix

prefixes :: [a] -> [[a]]
prefixes = foldr (\x xs -> [x] : map (x :) xs) [] 

prefix :: [Music a] -> Music a
prefix mel = 
    let
        m1 = line (concat (prefixes mel))
        m2 = transpose 12 (line (concat (prefixes (reverse mel))))
        m = instrument Flute m1 :=: instrument VoiceOohs m2 
    in m :+: transpose 5 m :+: m

-- Modify prefix to take the instruments and transposition as arguments
prefix1 :: InstrumentName -> InstrumentName -> Int -> [Music a] -> Music a
prefix1 instrument1 instrument2 n mel = 
    let
        m1 = line (concat (prefixes mel))
        m2 = transpose 12 (line (concat (prefixes (reverse mel))))
        m = instrument instrument1 m1 :=: instrument instrument2 m2 
    in m :+: transpose n m :+: m

mel1 = [c 5 en, e 5 sn, g 5 en, b 5 sn, a 5 en, f 5 sn, d 5 en, b 4 sn, c 5 en]
mel2 = [c 5 sn, e 5 sn, g 5 sn, b 5 sn, a 5 sn, f 5 sn, d 5 sn, b 4 sn, c 5 sn]
cmaj = [c 4 en, d 4 en, e 4 en, f 4 en, g 4 en, a 4 en, b 4 en, c 5 en]  



