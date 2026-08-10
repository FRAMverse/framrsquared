# filter_sport() returns expected IDs for CHINOOK

    Code
      filter_sport(make_fishery_df("CHINOOK"), return_ids = TRUE)
    Output
       [1]  3  8 11 13 14 15 18 22 27 29 31 33 35 48 60 62 72 36 42 45 53 54 56 57 64
      [26] 67

# filter_sport() returns expected IDs for COHO

    Code
      filter_sport(make_fishery_df("COHO"), return_ids = TRUE)
    Output
       [1]   3   5   7  15  17  19  21  23  24  28  29  31  33  37  40  41  45  46  48
      [20]  49  51  54  58  59  60  61  62  65  66  67  70  73  76  89  90  91  92  93
      [39]  94  95  99 100 106 107 108 115 116 117 118 127 128 129 135 136 149 150 151
      [58] 152 163 164 165 166 169 186 187 188 189 190 191 192 193

# filter_net() returns expected IDs for CHINOOK

    Code
      filter_net(make_fishery_df("CHINOOK"), return_ids = TRUE)
    Output
       [1]  3  8 11 13 14 15 18 22 27 29 31 33 35 48 60 62 72 36 42 45 53 54 56 57 64
      [26] 67

# filter_net() returns expected IDs for COHO

    Code
      filter_net(make_fishery_df("COHO"), return_ids = TRUE)
    Output
       [1]   3   5   7  15  17  19  21  23  24  28  29  31  33  37  40  41  45  46  48
      [20]  49  51  54  58  59  60  61  62  65  66  67  70  73  76  80  89  90  91  92
      [39]  93  94  95  99 100 106 107 108 115 116 117 118 127 129 135 136 149 150 151
      [58] 152 163 164 165 166 169 186 187 188 189 190 191 192 193

# filter_puget_sound() returns expected IDs for CHINOOK

    Code
      filter_puget_sound(make_fishery_df("CHINOOK"), return_ids = TRUE)
    Output
       [1] 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
      [26] 61 62 63 64 65 66 67 68 69 70 71

# filter_puget_sound() returns expected IDs for COHO

    Code
      filter_puget_sound(make_fishery_df("COHO"), return_ids = TRUE)
    Output
       [1]  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94
      [20]  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113
      [39] 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132
      [58] 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151
      [77] 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166

# filter_wa() returns expected IDs for CHINOOK

    Code
      filter_wa(make_fishery_df("CHINOOK"), return_ids = TRUE)
    Output
       [1] 16 17 18 19 20 21 22 23 24 25 26 27 28 29 36 37 38 39 40 41 42 43 44 45 46
      [26] 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71

# filter_wa() returns expected IDs for COHO

    Code
      filter_wa(make_fishery_df("COHO"), return_ids = TRUE)
    Output
        [1]  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40
       [19]  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58
       [37]  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76
       [55]  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94
       [73]  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112
       [91] 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130
      [109] 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148
      [127] 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166

# filter_bc() returns expected IDs for CHINOOK

    Code
      filter_bc(make_fishery_df("CHINOOK"), return_ids = TRUE)
    Output
       [1]  4  5  6  7  8  9 10 11 12 13 14 15

# filter_bc() returns expected IDs for COHO

    Code
      filter_bc(make_fishery_df("COHO"), return_ids = TRUE)
    Output
       [1] 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185
      [20] 186 187 188 189 190 191 192 193

# filter_ak() returns expected IDs for CHINOOK

    Code
      filter_ak(make_fishery_df("CHINOOK"), return_ids = TRUE)
    Output
      [1] 1 2 3

# filter_ak() returns expected IDs for COHO

    Code
      filter_ak(make_fishery_df("COHO"), return_ids = TRUE)
    Output
      [1] 194 195 196 197 198

# filter_ca() returns expected IDs for CHINOOK

    Code
      filter_ca(make_fishery_df("CHINOOK"), return_ids = TRUE)
    Output
      [1] 32 33 34

# filter_ca() returns expected IDs for COHO

    Code
      filter_ca(make_fishery_df("COHO"), return_ids = TRUE)
    Output
      [1] 1 2 3 4 5 6 7 8

# filter_or() returns expected IDs for CHINOOK

    Code
      filter_or(make_fishery_df("CHINOOK"), return_ids = TRUE)
    Output
      [1] 28 29 30 31 32 33

# filter_or() returns expected IDs for COHO

    Code
      filter_or(make_fishery_df("COHO"), return_ids = TRUE)
    Output
       [1]  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29
      [26] 30 31 32

# filter_coast() returns expected IDs for CHINOOK

    Code
      filter_coast(make_fishery_df("CHINOOK"), return_ids = TRUE)
    Output
       [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
      [26] 26 27 28 29 30 31 32 33 34 35

# filter_coast() returns expected IDs for COHO

    Code
      filter_coast(make_fishery_df("COHO"), return_ids = TRUE)
    Output
       [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 33 34 35
      [26] 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
      [51] 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75

# filter_marine() returns expected IDs for CHINOOK

    Code
      filter_marine(make_fishery_df("CHINOOK"), return_ids = TRUE)
    Output
       [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
      [26] 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
      [51] 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71

# filter_marine() returns expected IDs for COHO

    Code
      filter_marine(make_fishery_df("COHO"), return_ids = TRUE)
    Output
        [1]   3   4   5   6   7   8  15  16  17  18  19  20  21  22  33  34  35  36
       [19]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  79  80  81  82
       [37]  83  87  88  91  92  93  96  97 101 102 105 106 107 109 110 111 112 115
       [55] 118 119 120 121 122 123 124 129 130 131 132 133 136 137 138 139 140 141
       [73] 142 143 144 145 146 152 153 154 155 156 157 158 159 160 170 171 172 173
       [91] 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191
      [109] 192 193 194 195 196 197 198

# filter_commercial_wa_nt() returns expected IDs for CHINOOK

    Code
      filter_commercial_wa_nt(make_fishery_df("CHINOOK"), return_ids = TRUE)
    Output
       [1] 58 65 68 70 37 39 43 46 49 51

# filter_commercial_wa_nt() returns expected IDs for COHO

    Code
      filter_commercial_wa_nt(make_fishery_df("COHO"), return_ids = TRUE)
    Output
       [1]  80  82  87  96 101 109 111 119 121 123 130 132 137 139 141 143 145 153 155
      [20] 157 159

# filter_stt() returns expected IDs for COHO

    Code
      filter_stt(make_fishery_df("COHO"), return_ids = TRUE)
    Output
       [1] 33 37 40 41 34 35 38 42 36 39 43 17 18 19 29 21 22

# filter_stt_nt() returns expected IDs for COHO

    Code
      filter_stt_nt(make_fishery_df("COHO"), return_ids = TRUE)
    Output
       [1] 33 37 40 41 34 35 38 42 17 18 19 29 21 22

# filter_hatchery() returns expected IDs for COHO

    Code
      filter_hatchery(make_stock_df("COHO"), return_ids = TRUE)
    Output
        [1]   3   4   5   6   7   8   9  10  15  16  19  20  21  22  25  26  27  28
       [19]  31  32  33  34  37  38  39  40  41  42  47  48  49  50  53  54  57  58
       [37]  65  66  67  68  71  72  73  74  77  78  79  80  83  84  87  88  91  92
       [55]  95  96  99 100 103 104 109 110 113 114 119 120 125 126 129 130 133 134
       [73] 137 138 141 142 143 144 147 148 151 152 155 156 159 160 163 164 165 166
       [91] 167 168 175 176 177 178 181 182 185 186 189 190 191 192 193 194 197 198
      [109] 201 202 205 206 209 210 213 214 217 218 221 222 225 226 229 230

# filter_wild() returns expected IDs for COHO

    Code
      filter_wild(make_stock_df("COHO"), return_ids = TRUE)
    Output
        [1]   1   2  11  12  13  14  17  18  23  24  29  30  35  36  43  44  45  46
       [19]  51  52  55  56  59  60  61  62  63  64  69  70  75  76  81  82  85  86
       [37]  89  90  93  94  97  98 101 102 105 106 107 108 111 112 115 116 117 118
       [55] 121 122 123 124 127 128 131 132 135 136 139 140 145 146 149 150 153 154
       [73] 157 158 161 162 169 170 171 172 173 174 179 180 183 184 187 188 195 196
       [91] 199 200 203 204 207 208 211 212 215 216 219 220 223 224 227 228 231 232

# filter_mixed() returns expected IDs for COHO

    Code
      filter_mixed(make_stock_df("COHO"), return_ids = TRUE)
    Output
       [1] 233 234 235 236 237 238 239 240 241 242 243 244 245 246

