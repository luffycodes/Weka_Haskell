module Tester where

import Structures
import C45

decideProp = (map (classify tree) unlabeled) == labels where
    tree = build 2 dataset

-- sunny, overcast, rainy
outlook = Nominal { attName = "outlook", possibleValues=[1, 2, 3] }
-- hot, mild, cool
temperature = Nominal { attName ="temperature", possibleValues=[1,2, 3] }
-- high, normal
humidity = Nominal { attName ="humidity", possibleValues=[1,2] }
-- true, false
windy = Nominal { attName ="windy", possibleValues=[1,2]}

atts = [outlook,temperature,humidity,windy]

dataset = zipWith Labelled labels unlabeled
labels = ["0","0","1","1","1","0","1","0","1","1","1","1","1","0"]
unlabeled = 
    [Datum { attributes = [(outlook,1), 
            (temperature,1), 
            (humidity,1),
            (windy,2)]},
    Datum { attributes = [(outlook,1), 
        (temperature,1), 
        (humidity, 1), 
        (windy,1)]},
    Datum { attributes = [(outlook,2),
        (temperature,1), 
        (humidity, 1), 
        (windy, 2)]},
    Datum { attributes = [(outlook,3), 
        (temperature,2), 
        (humidity, 1), 
        (windy,2)]},
    Datum { attributes = [(outlook,3), 
        (temperature, 3), 
        (humidity, 2), 
        (windy,2)]},
    Datum { attributes = [(outlook, 3), 
        (temperature,3), 
        (humidity, 2), 
        (windy, 1)]},
    Datum { attributes = [(outlook, 2), 
        (temperature, 3), 
        (humidity, 2), 
        (windy,1)]},
    Datum { attributes = [(outlook, 1), 
        (temperature, 2), 
        (humidity, 1), 
        (windy, 2)]},
    Datum { attributes = [(outlook, 1), 
        (temperature, 3), 
        (humidity, 2), 
        (windy, 2)]},
    Datum { attributes = [(outlook, 3), 
        (temperature, 2), 
        (humidity, 2), 
        (windy, 2)]},
    Datum { attributes = [(outlook, 1), 
        (temperature, 2), 
        (humidity, 2), 
        (windy, 1)]},
    Datum { attributes = [(outlook, 2), 
        (temperature, 2), 
        (humidity, 1), 
        (windy, 1)]},
    Datum { attributes = [(outlook, 2), 
        (temperature, 1), 
        (humidity, 2), 
        (windy, 2)]},
    Datum { attributes = [(outlook, 3), 
        (temperature, 2), 
        (humidity, 1), 
        (windy,1)]}]

