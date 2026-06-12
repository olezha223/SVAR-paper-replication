
**Структурная форма SVAR**

$$A_0 z_t = \alpha + \sum_{i=1}^{24} A_i z_{t-i} + \varepsilon_t, \quad \varepsilon_t \sim \text{i.i.d.}, \quad \text{Cov}(\varepsilon_t) = I$$

**Вектор эндогенных переменных**

$$z_t = \begin{pmatrix} \Delta prod_t \\ rea_t \\ rpo_t \\ ret_t \end{pmatrix}, \quad \varepsilon_t = \begin{pmatrix} \varepsilon_{1t} \\ \varepsilon_{2t} \\ \varepsilon_{3t} \\ \varepsilon_{4t} \end{pmatrix}$$

**Рекурсивная идентификация: связь приведённых и структурных шоков**

$$e_t = A_0^{-1} \varepsilon_t = \begin{pmatrix} e_{1t}^{\text{global oil production}} \\ e_{2t}^{\text{global real activity}} \\ e_{3t}^{\text{real price of oil}} \\ e_{4t}^{\text{U.S. stock returns}} \end{pmatrix} = \begin{bmatrix} a_{11} & 0 & 0 & 0 \\ a_{21} & a_{22} & 0 & 0 \\ a_{31} & a_{32} & a_{33} & 0 \\ a_{41} & a_{42} & a_{43} & a_{44} \end{bmatrix} \begin{pmatrix} \varepsilon_{1t}^{\text{oil supply shock}} \\ \varepsilon_{2t}^{\text{aggregate demand shock}} \\ \varepsilon_{3t}^{\text{oil-specific demand shock}} \\ \varepsilon_{4t}^{\text{other shocks to stock returns}} \end{pmatrix}$$
