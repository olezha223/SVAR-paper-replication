Вы можете получить данные, полученные из открытых источников

Данные используем как в статье: за период 1974:1-2006:12

Не стали интерполировать назад $pro_t$, так как недостаточно подробно описана методология в оригинальной статье.

- [The aggregate U.S. real stock market return (`ret_t`)](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html)
- [US CPI](https://fred.stlouisfed.org/series/CPIAUCSL)
- [Real Global Economic Activity Index (`rea_t`)](https://www.dallasfed.org/research/igrea)
- [World production ($\Delta prod^{non-US}_t$)](https://www.eia.gov/international/data/world/petroleum-and-other-liquids/monthly-petroleum-and-other-liquids-production)
- [The real price of oil (`rpo_t`) is U.S. refiner acquisition cost of imported crude oil, from the U.S. Department of Energy](https://www.eia.gov/dnav/pet/pet_pri_rac2_a_epc0_pft_dpbbl_m.htm)

Результаты могут отличаться от исходных данных, так как, во-первых, в 2019 Killian выпустил исправленный IGREA индекс, во-вторых, мы используем очень сильную прокси к $ret_t$, обоснование выбора лежит в папке с обработкой данных на python
