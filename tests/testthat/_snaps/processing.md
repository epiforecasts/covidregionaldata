# process_regional.crd_level_1 works

    Code
      reproc_total
    Output
      # A tibble: 32 x 7
         estados        iso_3166_2 cases_total deaths_total recovered_total hosp_total
         <chr>          <chr>            <dbl>        <dbl>           <int>      <int>
       1 Ciudad de Mex~ MX-CMX          567778        28008               0          0
       2 Mexico         MX-MEX          220661        29313               0          0
       3 Guanajuato     MX-GUA          120957         9283               0          0
       4 Nuevo Leon     MX-NLE          116359         8477               0          0
       5 Jalisco        MX-JAL           79167        10679               0          0
       6 Puebla         MX-PUE           72354         9172               0          0
       7 Sonora         MX-SON           68263         6090               0          0
       8 Coahuila       MX-COA           65274         5718               0          0
       9 Queretaro      MX-QUE           58443         3476               0          0
      10 Tabasco        MX-TAB           58065         3742               0          0
      # ... with 22 more rows, and 1 more variable: tested_total <int>

# process_regional.crd_level_2 works

    Code
      reproc_total
    Output
      # A tibble: 34 x 9
         municipios inegi_code region_level_1      level_1_region_code cases_total
         <chr>      <chr>      <chr>               <chr>                     <dbl>
       1 Juarez     16046      Michoacan           MX-MIC                      170
       2 Jungapeo   16047      Michoacan           MX-MIC                       53
       3 Lagunillas 16048      Michoacan           MX-MIC                       29
       4 <NA>       <NA>       Aguascalientes      MX-AGU                        0
       5 <NA>       <NA>       Baja California     MX-BCN                        0
       6 <NA>       <NA>       Baja California Sur MX-BCS                        0
       7 <NA>       <NA>       Campeche            MX-CAM                        0
       8 <NA>       <NA>       Chiapas             MX-CHP                        0
       9 <NA>       <NA>       Chihuahua           MX-CHH                        0
      10 <NA>       <NA>       Ciudad de Mexico    MX-CMX                        0
      # ... with 24 more rows, and 4 more variables: deaths_total <dbl>,
      #   recovered_total <int>, hosp_total <int>, tested_total <int>

