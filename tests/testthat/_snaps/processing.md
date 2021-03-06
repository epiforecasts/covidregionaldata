# process_regional.crd_level_1 works

    Code
      reproc_total
    Output
      [38;5;246m# A tibble: 32 x 7[39m
         estados        iso_3166_2 cases_total deaths_total recovered_total hosp_total
         [3m[38;5;246m<chr>[39m[23m          [3m[38;5;246m<chr>[39m[23m            [3m[38;5;246m<dbl>[39m[23m        [3m[38;5;246m<dbl>[39m[23m           [3m[38;5;246m<int>[39m[23m      [3m[38;5;246m<int>[39m[23m
      [38;5;250m 1[39m Ciudad de Mex~ MX-CMX          [4m5[24m[4m6[24m[4m7[24m778        [4m2[24m[4m8[24m008               0          0
      [38;5;250m 2[39m Mexico         MX-MEX          [4m2[24m[4m2[24m[4m0[24m661        [4m2[24m[4m9[24m313               0          0
      [38;5;250m 3[39m Guanajuato     MX-GUA          [4m1[24m[4m2[24m[4m0[24m957         [4m9[24m283               0          0
      [38;5;250m 4[39m Nuevo Leon     MX-NLE          [4m1[24m[4m1[24m[4m6[24m359         [4m8[24m477               0          0
      [38;5;250m 5[39m Jalisco        MX-JAL           [4m7[24m[4m9[24m167        [4m1[24m[4m0[24m679               0          0
      [38;5;250m 6[39m Puebla         MX-PUE           [4m7[24m[4m2[24m354         [4m9[24m172               0          0
      [38;5;250m 7[39m Sonora         MX-SON           [4m6[24m[4m8[24m263         [4m6[24m090               0          0
      [38;5;250m 8[39m Coahuila       MX-COA           [4m6[24m[4m5[24m274         [4m5[24m718               0          0
      [38;5;250m 9[39m Queretaro      MX-QUE           [4m5[24m[4m8[24m443         [4m3[24m476               0          0
      [38;5;250m10[39m Tabasco        MX-TAB           [4m5[24m[4m8[24m065         [4m3[24m742               0          0
      [38;5;246m# ... with 22 more rows, and 1 more variable: tested_total <int>[39m

# process_regional.crd_level_2 works

    Code
      reproc_total
    Output
      [38;5;246m# A tibble: 34 x 9[39m
         municipios inegi_code region_level_1      level_1_region_code cases_total
         [3m[38;5;246m<chr>[39m[23m      [3m[38;5;246m<chr>[39m[23m      [3m[38;5;246m<chr>[39m[23m               [3m[38;5;246m<chr>[39m[23m                     [3m[38;5;246m<dbl>[39m[23m
      [38;5;250m 1[39m Juarez     16046      Michoacan           MX-MIC                      170
      [38;5;250m 2[39m Jungapeo   16047      Michoacan           MX-MIC                       53
      [38;5;250m 3[39m Lagunillas 16048      Michoacan           MX-MIC                       29
      [38;5;250m 4[39m [31mNA[39m         [31mNA[39m         Aguascalientes      MX-AGU                        0
      [38;5;250m 5[39m [31mNA[39m         [31mNA[39m         Baja California     MX-BCN                        0
      [38;5;250m 6[39m [31mNA[39m         [31mNA[39m         Baja California Sur MX-BCS                        0
      [38;5;250m 7[39m [31mNA[39m         [31mNA[39m         Campeche            MX-CAM                        0
      [38;5;250m 8[39m [31mNA[39m         [31mNA[39m         Chiapas             MX-CHP                        0
      [38;5;250m 9[39m [31mNA[39m         [31mNA[39m         Chihuahua           MX-CHH                        0
      [38;5;250m10[39m [31mNA[39m         [31mNA[39m         Ciudad de Mexico    MX-CMX                        0
      [38;5;246m# ... with 24 more rows, and 4 more variables: deaths_total <dbl>,[39m
      [38;5;246m#   recovered_total <int>, hosp_total <int>, tested_total <int>[39m

