razvedanalis
================

# Подготовка к работе

## Загрузка пакетов

### Общая тема оформления для всех графиков и отключение экспоненциальной записи чисел

``` r
theme_set(theme_classic(base_size = 12))
options(scipen = 999) 
```

# Загрузка данных

# Типы данных

    ## Rows: 2,390
    ## Columns: 29
    ## $ meat               <dbl> 0.20, 3.30, 4.50, 1.50, 1.60, 1.50, 1.60, 4.00, 1.6~
    ## $ fish               <dbl> 1.0, 0.0, 1.5, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1~
    ## $ eggs               <int> 0, 20, 10, 20, 0, 0, 10, 0, 0, 50, 10, 20, 0, 20, 2~
    ## $ milk               <dbl> 0.00, 1.00, 3.00, 2.00, 2.50, 7.00, 2.40, 5.00, 2.0~
    ## $ vegetables         <dbl> 5.0, 1.5, 1.0, 12.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, ~
    ## $ fruits_and_berries <dbl> 1.0, 0.3, 0.0, 1.5, 0.0, 0.0, 0.0, 1.0, 0.0, 1.5, 0~
    ## $ cereals            <dbl> 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 3.0, 1.0, 0.0, 1~
    ## $ flour              <dbl> 1.30, 1.50, 1.40, 1.20, 3.50, 8.80, 4.20, 6.40, 3.2~
    ## $ sweets             <dbl> 0.0, 1.2, 2.0, 2.0, 1.5, 7.0, 1.0, 2.0, 1.0, 4.0, 0~
    ## $ softdrinks         <dbl> 0.00, 0.00, 0.00, 0.00, 0.15, 0.00, 0.00, 0.25, 0.0~
    ## $ alcodrinks         <dbl> 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0~
    ## $ sum_rub_buy        <dbl> 336, 1298, 1834, 924, 1208, 2644, 1331, 3528, 844, ~
    ## $ income             <dbl> 28161, 36900, 68070, 59000, 25000, 105400, 28900, 6~
    ## $ diplom             <int> 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, ~
    ## $ car                <int> 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, ~
    ## $ plot_bi            <dbl> NA, 1, 1, 1, 1, 1, 1, 1, NA, 1, NA, NA, 1, 1, 1, NA~
    ## $ plot_size          <dbl> NA, 12.0, 10.4, 6.0, 6.0, 25.0, 2.0, 10.0, NA, 8.0,~
    ## $ famsize            <int> 1, 2, 2, 2, 1, 3, 1, 2, 1, 2, 1, 2, 2, 2, 1, 1, 2, ~
    ## $ water              <int> 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, ~
    ## $ sanitation         <int> 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, ~
    ## $ fridge             <int> 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ~
    ## $ internet           <int> 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, ~
    ## $ pfuel              <dbl> NA, NA, NA, NA, NA, 5000, NA, 4000, NA, 3000, NA, 6~
    ## $ ptransp            <dbl> NA, 3000, NA, 1200, NA, NA, NA, NA, NA, NA, NA, 430~
    ## $ phome              <dbl> 6200.00, 10000.00, NA, 5800.00, 7000.00, 11000.00, ~
    ## $ debt               <int> 0, 0, 0, 0, 0, 500000, 100000, 0, 0, 0, 0, 0, 0, 20~
    ## $ govsubs            <dbl> 26732, 26300, 66100, 29720, 23000, 105400, 26900, 5~
    ## $ nongovsubs         <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
    ## $ inval              <int> 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, ~

# Описательные статистики

    ##       meat             fish             eggs             milk       
    ##  Min.   : 0.000   Min.   :0.0000   Min.   :  0.00   Min.   : 0.000  
    ##  1st Qu.: 1.500   1st Qu.:0.0000   1st Qu.:  0.00   1st Qu.: 1.550  
    ##  Median : 2.800   Median :0.0000   Median : 10.00   Median : 3.000  
    ##  Mean   : 3.431   Mean   :0.5064   Mean   : 10.78   Mean   : 3.738  
    ##  3rd Qu.: 4.900   3rd Qu.:1.0000   3rd Qu.: 20.00   3rd Qu.: 5.000  
    ##  Max.   :27.000   Max.   :7.0000   Max.   :100.00   Max.   :26.000  
    ##                                                                     
    ##    vegetables     fruits_and_berries    cereals            flour      
    ##  Min.   :  0.00   Min.   : 0.000     Min.   : 0.0000   Min.   : 0.00  
    ##  1st Qu.:  0.00   1st Qu.: 0.000     1st Qu.: 0.0000   1st Qu.: 1.40  
    ##  Median :  2.00   Median : 2.000     Median : 0.0000   Median : 2.70  
    ##  Mean   :  4.76   Mean   : 2.509     Mean   : 0.6427   Mean   : 4.13  
    ##  3rd Qu.:  5.50   3rd Qu.: 3.000     3rd Qu.: 1.0000   3rd Qu.: 5.00  
    ##  Max.   :352.00   Max.   :30.000     Max.   :12.0000   Max.   :56.45  
    ##                                                                       
    ##      sweets         softdrinks        alcodrinks       sum_rub_buy   
    ##  Min.   : 0.000   Min.   : 0.0000   Min.   : 0.0000   Min.   :    0  
    ##  1st Qu.: 0.500   1st Qu.: 0.0000   1st Qu.: 0.0000   1st Qu.: 1376  
    ##  Median : 1.300   Median : 0.0000   Median : 0.0000   Median : 2270  
    ##  Mean   : 1.983   Mean   : 0.6139   Mean   : 0.4633   Mean   : 2735  
    ##  3rd Qu.: 2.500   3rd Qu.: 0.3000   3rd Qu.: 0.0000   3rd Qu.: 3550  
    ##  Max.   :35.000   Max.   :30.2000   Max.   :28.0000   Max.   :18844  
    ##                                                                      
    ##      income           diplom            car            plot_bi     
    ##  Min.   :     0   Min.   :0.0000   Min.   :0.0000   Min.   :1.000  
    ##  1st Qu.: 30719   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:1.000  
    ##  Median : 50860   Median :0.0000   Median :0.0000   Median :1.000  
    ##  Mean   : 62665   Mean   :0.4255   Mean   :0.4205   Mean   :1.004  
    ##  3rd Qu.: 80000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.000  
    ##  Max.   :866000   Max.   :1.0000   Max.   :1.0000   Max.   :2.000  
    ##                                                     NA's   :1266   
    ##    plot_size          famsize           water         sanitation   
    ##  Min.   :   0.50   Min.   : 1.000   Min.   :1.000   Min.   :1.000  
    ##  1st Qu.:   5.00   1st Qu.: 1.000   1st Qu.:1.000   1st Qu.:1.000  
    ##  Median :   7.00   Median : 2.000   Median :1.000   Median :1.000  
    ##  Mean   :  13.48   Mean   : 2.572   Mean   :1.104   Mean   :1.256  
    ##  3rd Qu.:  15.00   3rd Qu.: 3.000   3rd Qu.:1.000   3rd Qu.:2.000  
    ##  Max.   :1430.00   Max.   :13.000   Max.   :2.000   Max.   :2.000  
    ##  NA's   :1271                                                      
    ##      fridge         internet          pfuel          ptransp     
    ##  Min.   :1.000   Min.   :0.0000   Min.   :  430   Min.   :    0  
    ##  1st Qu.:1.000   1st Qu.:0.0000   1st Qu.: 3000   1st Qu.:  350  
    ##  Median :1.000   Median :1.0000   Median : 4000   Median :  850  
    ##  Mean   :1.272   Mean   :0.7046   Mean   : 4796   Mean   : 1377  
    ##  3rd Qu.:2.000   3rd Qu.:1.0000   3rd Qu.: 6000   3rd Qu.: 1800  
    ##  Max.   :2.000   Max.   :1.0000   Max.   :60000   Max.   :50000  
    ##                                   NA's   :1427    NA's   :938    
    ##      phome              debt            govsubs         nongovsubs      
    ##  Min.   :  163.9   Min.   :      0   Min.   :     0   Min.   :    0.00  
    ##  1st Qu.: 2800.0   1st Qu.:      0   1st Qu.:     0   1st Qu.:    0.00  
    ##  Median : 4000.0   Median :      0   Median : 18000   Median :    0.00  
    ##  Mean   : 4576.2   Mean   :  84960   Mean   : 19887   Mean   :   87.66  
    ##  3rd Qu.: 5845.0   3rd Qu.:      0   3rd Qu.: 29608   3rd Qu.:    0.00  
    ##  Max.   :35500.0   Max.   :7380000   Max.   :106595   Max.   :50000.00  
    ##  NA's   :150                                                            
    ##      inval       
    ##  Min.   :0.0000  
    ##  1st Qu.:0.0000  
    ##  Median :0.0000  
    ##  Mean   :0.1607  
    ##  3rd Qu.:0.0000  
    ##  Max.   :1.0000  
    ## 

# Графики

## Выбросы: доходы в руб.

    ## Warning: Continuous x aesthetic -- did you forget aes(group=...)?

![](krrazvedanaliz_files/figure-gfm/4-1.png)<!-- -->

## Выбросы: расходы на питание в руб.

    ## Warning: Continuous x aesthetic -- did you forget aes(group=...)?

![](krrazvedanaliz_files/figure-gfm/5-1.png)<!-- -->

## Плотность распределения: доход в руб.

![](krrazvedanaliz_files/figure-gfm/6-1.png)<!-- -->

## Плотность распределения: расходы на питание в руб.

![](krrazvedanaliz_files/figure-gfm/7-1.png)<!-- -->

## Корреляция (верхняя часть рисунка)

## Плотность распределения (диагональ)

## Совместное распределение (нижняя часть рисунка)

## Первая переменная: доход в руб.; остальные: потребление продовольственных товаров в руб.

![](krrazvedanaliz_files/figure-gfm/8-1.png)<!-- -->

## Совместное распределение дохода в руб. и расходы на питание в руб.

![](krrazvedanaliz_files/figure-gfm/9-1.png)<!-- -->
