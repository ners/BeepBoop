module Main where

import Data.Int

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Codec.Audio.Wave as W
import qualified Codec.Audio.FLAC.StreamEncoder as FLAC

type Sample = Int16

amplitude :: Num a => a
amplitude = 32767

samplingRate :: Num hz => hz
samplingRate = 11025

msToSamples :: Int -> Int
msToSamples ms = (ms * samplingRate) `div` 1000

note :: Double -> Int -> [Sample]
note hz ms = take num $ round . (amplitude *) . sin <$> ts
    where
        num = msToSamples ms
        dt = hz * 2 * pi / samplingRate
        ts = [0,dt..]

silence :: Int -> [Sample]
silence ms = replicate num 0
    where num = msToSamples ms

a5 = 880
b5 = 987.77
c5 = 523.25
d5 = 587.33
e5 = 659.25
f5 = 698.46
g5 = 783.99

-- 3 pulses: C5, G5, D5
-- ts = 250 ms, 250 ms
-- td = 250 ms
-- tb = 30 s
mediumPrioritySignal :: [Sample]
mediumPrioritySignal = cycle $ concat
    [ note c5 250
    , silence 250
    , note g5 250
    , silence 250
    , note d5 250
    , silence 30000
    ]

-- 10 pulses: C5, A5, D5, E5, F5, C5, A5, D5, E5, F5
-- ts = 0.1 s, 0.1 s, 0.3 s, 0.1 s, 1 s, 0.1 s, 0.1 s, 0.3 s, 0.1 s, 1 s
-- td = 0.2 s
-- tb = 5 s
highPrioritySignal :: [Sample]
highPrioritySignal = cycle $ concat
    [ note c5 200
    , silence 100
    , note a5 200
    , silence 100
    , note d5 200
    , silence 300
    , note e5 200
    , silence 100
    , note f5 200
    , silence 100
    , note c5 200
    , silence 100
    , note a5 200
    , silence 100
    , note d5 200
    , silence 300
    , note e5 200
    , silence 100
    , note f5 200
    , silence 5000
    ]

saveSignal :: FilePath -> [Sample] -> IO ()
saveSignal filename samples = do
    let numSamples = length samples
    let wave = W.Wave
            { W.waveFileFormat = W.WaveVanilla
            , W.waveSampleRate = samplingRate
            , W.waveSampleFormat = W.SampleFormatPcmInt 16
            , W.waveChannelMask = W.speakerMono
            , W.waveDataOffset = 0
            , W.waveDataSize = fromIntegral $ numSamples * 2
            , W.waveSamplesTotal = fromIntegral numSamples
            , W.waveOtherChunks = []
            }
    let wavfile = filename <> ".wav"
    let flacfile = filename <> ".flac"
    W.writeWaveFile wavfile wave $ \handle -> B.hPutBuilder handle (mconcat $ B.int16LE <$> samples)
    FLAC.encodeFlac FLAC.defaultEncoderSettings wavfile flacfile

main :: IO ()
main = do
    saveSignal "medium" $ take 5000 mediumPrioritySignal
    saveSignal "high" $ take 5000 highPrioritySignal
