
data Value = Object (Map Text Value)
           | Array [Value]
           | String Text
           | Number Scientific
           | Bool Bool
           | Null


