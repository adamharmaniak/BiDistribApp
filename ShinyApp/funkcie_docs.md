# Dokumentácia funkcií

## `model_mixture_density()`

**Popis:**  
Táto funkcia modeluje združenú hustotu pravdepodobnosti pre dvojicu diskrétnej a spojitej náhodnej premennej. Výsledná hustota sa konštruuje ako vážený súčet podmienených hustôt spojitej premennej v jednotlivých kategóriách diskrétnej premennej.

---

**Vstupné argumenty:**

| Argument         | Typ       | Popis |
|------------------|-----------|-------|
| `data`           | `data.frame` | Tabuľka s dátami |
| `discrete_vars`  | `character` | Názov stĺpca s diskrétnou premennou |
| `continuous_vars`| `character` | Názov stĺpca so spojitou premennou |
| `model_type`     | `character` | Typ modelu hustoty: `"kernel"` (jadrové vyhladzovanie), `"normal"` (normálne rozdelenie), alebo `"t"`(t-rozdelenie) |
| `bw`             | `numeric` alebo `NULL` | Rozsah vyhladzovania pre KDE (ak je `NULL`, použije sa `bw.nrd0`) |

---

**Použité modely:**

- **Kernel Density Estimation (KDE):**  
  Jadrovo vyhladený odhad hustoty pre každú kategóriu, vážený podľa početnosti kategórie.

- **Normálne rozdelenie:**  
  Odhad hustoty pomocou normálnej distribúcie s empirickým priemerom a štandardnou odchýlkou.

- **t-rozdelenie:**  
  Robustnejší odhad hustoty v prípade malého počtu pozorovaní.

---

**Výstup:**

Funkcia vracia `list` s nasledujúcimi položkami:

- `density_data`: `data.frame` s odhadnutou združenou hustotou (stĺpce: `Continuous_Var`, `Density`, `Discrete_Var`)
- `category_probs`: pravdepodobnosti kategórií diskrétnej premennej
- `summary_info`: zoznam s metainformáciami o modelovaní (typ modelu, bandwidth, integrál hustoty, kategórie)
- `discrete_var`, `continuous_var`: názvy použitých premenných
- `category_colors`: farebné priradenia kategóriám pre vizualizáciu
- `data`: prefiltrovaná dátová tabuľka
- `vector_type`: `"mix"` — označuje zmiešaný typ rozdelenia

---

**Poznámky:**

- Funkcia automaticky ignoruje záznamy s `NA`.
- Kategórie s príliš málo dátami (< 2 pozorovania alebo nulová smerodajná odchýlka) sú ošetrené zvlášť nulovou hustotou.
- Pri type `"kernel"` sú použité rôzne rozsahy vyhladzovania pre jednotlivé kategórie (ak nie je `bw` zadané užívateľom).
- Vizualizácia prebieha v renderovacej funkcii `render_mixture_density()`, ktorá zobrazuje aj 2D aj 3D vizualizáciu podľa voľby užívateľa

---


## `model_continuous_density()`

**Popis:**  
Funkcia odhaduje združenú hustotu pravdepodobnosti pre dve spojité náhodné premenné pomocou neparametrického alebo parametrického modelu.

---

**Vstupné argumenty:**

| Argument         | Typ         | Popis |
|------------------|--------------|--------|
| `data`           | `data.frame` | Dátová tabuľka obsahujúca aspoň dve spojité premenné |
| `continuous_vars`| `character[2]` | Mená dvoch spojitých premenných |
| `model_type`     | `character` | Typ hustotného modelu: `"kernel"` (neparametrický), `"normal"`, alebo `"t"` |

---

**Použité modely:**

- **Kernel Density Estimation (KDE)**  
  Používa funkciu `MASS::kde2d()` na neparametrický odhad hustoty v dvojrozmernom priestore.

- **Normálne rozdelenie**  
  Hustota odhadnutá pomocou bivariantného normálneho rozdelenia založeného na priemeroch, smerodajných odchýlkach a Pearsonovej korelácii medzi premennými.

- **t-rozdelenie**  
  Robustný odhad hustoty s využitím bivariantného t-rozdelenia s počtom stupňov voľnosti `df = n - 1`.

---

**Výstup:**

Funkcia vracia `list` s nasledovnými položkami:

- `x_vals`, `y_vals`: Vektory hodnôt (rozsahy osi X a Y)
- `z_matrix`: Matica hodnôt združenej hustoty (rozmer 100×100)
- `model_type`: Použitý typ modelu (`"kernel"`, `"normal"` alebo `"t"`)
- `continuous_vars`: Mená použitých spojitých premenných
- `params`: Parametre modelu (napr. stredné hodnoty, smerodajné odchýlky, korelácia, df)
- `summary_info`: Tibble s prehľadom základných štatistík
- `vector_type`: `"continuous"` – označuje, že ide o model dvoch spojitých premenných

---

**Poznámky:**

- Pri KDE sa používa 100×100 bodov na mriežke.
- Ak sa použije `"normal"` alebo `"t"`, predpokladá sa, že premenné sú numerické a neobsahujú len konštanty.
- t-rozdelenie je vhodnejšie pri malom počte pozorovaní alebo pri prítomnosti odľahlých hodnôt.
- Vizualizácia prebieha v renderovacej funkcii `render_continuous_density()`, ktorá zobrazuje aj 2D aj 3D vizualizáciu podľa voľby užívateľa

---


## `model_continuous_density_copula()`

**Popis:**  
Funkcia modeluje združenú hustotu pravdepodobnosti pre dvojicu spojitých premenných pomocou **rozkladu na marginálne hustoty a kopulovú funkciu**. Podporuje neparametrické, parametrické a hybridné prístupy pre marginálne rozdelenia a rôzne typy kopúl.

---

**Vstupné argumenty:**

| Argument            | Typ           | Popis |
|---------------------|----------------|--------|
| `data`              | `data.frame`   | Dátová tabuľka so spojitými premennými |
| `continuous_vars`   | `character[2]` | Mená dvoch spojitých premenných |
| `model_type`        | `character`    | Typ modelu: `"nonparametric"`, `"parametric"` alebo `"hybrid"` |
| `copula_type`       | `character`    | Typ kopuly: `"empirical (beta)"`, `"Clayton"`, `"Gumbel"`, `"Frank"`, `"Joe"`, `"t"` |
| `marginal_densities`| `character[2]` | Typy marginálnych rozdelení: `"normal"`, `"t"`, `"log_normal"`, `"KDE"` (povinné pri `"parametric"` alebo `"hybrid"` modeloch) |

---

**Modelovacie režimy:**

- **`nonparametric`**  
  - Marginálne hustoty aj CDF sa odhadujú pomocou KDE.  
  - Používa sa *empirická beta kopula* (`empCopula`).  
  - Hustota sa počíta ako:  
    \[
    f_{X,Y}(x,y) = c(u_1, u_2) \cdot f_X(x) \cdot f_Y(y)
    \]  
    kde \( u_i = F_i(x_i) \) a \( c(u_1, u_2) \) je hustota kopuly.

- **`parametric`**  
  - Marginálne rozdelenia sú zadané (napr. normálne, t, log-normálne).  
  - Kopula sa odhaduje pomocou maximum likelihood (`fitCopula()`).

- **`hybrid`**  
  - Marginálne rozdelenia môžu byť rôzne pre každú premennú (napr. `normal` + `KDE`).  
  - Kopula môže byť *empirická* alebo *parametrická*.

---

**Výstup:**

Funkcia vracia `list` obsahujúci:

- `x_vals`, `y_vals`: Vektory hodnôt (sieť pre os X a Y)
- `z_matrix`: Matica hodnôt združenej hustoty (100×100)
- `copula_type`: Použitá kopula
- `model_type`: Použitý režim modelovania
- `marginal_densities`: Použité marginálne modely
- `copula_model_fitted`: Fitted kopula objekt (`copula`)
- `rho_copula`, `df_copula`: Odhadnuté parametre kopuly (ak sú dostupné)
- `bw_x`, `bw_y`: Použité šírky pásma pre KDE (ak sa použili)
- `continuous_vars`: Mená modelovaných premenných
- `vector_type`: `"continuous_copula"`

---

**Poznámky:**

- Funkcia počíta hustotu ako súčin kopuly a marginálnych hustôt.
- Empirická kopula `"empirical (beta)"` vyžaduje neparametrické alebo hybridné modely.
- Ak sa používa `"parametric"` alebo `"hybrid"` režim, je povinné špecifikovať `marginal_densities`.
- Funkcia využíva balíky ako **`copula`**, **`stats`**, **`tibble`** a **`MASS`**.
- Vizualizácia prebieha v renderovacej funkcii `render_continuous_density_copula()`, ktorá zobrazuje aj 2D aj 3D vizualizáciu podľa voľby užívateľa

---


## `model_joint_pmf()`

**Popis:**  
Funkcia modeluje **pravdepodobnostnú funkciu (PMF)** pre dvojicu diskrétnych premenných. Výstupom je tabuľka všetkých kombinácií hodnôt s ich pravdepodobnosťami, spolu s prehľadnou sumarizačnou tabuľkou v `gt` formáte.

---

**Vstupné argumenty:**

| Argument         | Typ             | Popis |
|------------------|------------------|--------|
| `data`           | `data.frame`     | Dátová tabuľka s diskrétnymi premennými |
| `discrete_vars`  | `character[2]`   | Mená dvoch diskrétnych premenných |

---

**Výstup:**

Funkcia vracia `list` obsahujúci:

- `tab`: Tabuľka obsahujúca všetky kombinácie hodnôt `X × Y`, ich absolútny výskyt a vypočítanú pravdepodobnosť `Probability`
- `x_labels`, `y_labels`: Názvy (úrovne) premennej `X` a `Y`
- `discrete_vars`: Názvy zadaných diskrétnych premenných
- `summary`: Prehľadná `gt` sumarizačná tabuľka obsahujúca:
  - Typ modelu (`discrete`)
  - Názvy premenných `X`, `Y`
  - Počet úrovní každej premennej
  - Počet stavov v kombinácii `X × Y`
  - Zoznam pravdepodobností v markdown bloku
  - Celkový súčet pravdepodobností (mal by byť ~1)
- `vector_type`: `"discrete"`

---

**Poznámky:**

- Funkcia očakáva, že obe premenné sú diskrétne (faktory alebo znaky).
- Pravdepodobnosti sú vypočítané ako pomer frekvencie danej kombinácie ku všetkým záznamom.
- Premenné sú automaticky skonvertované na faktory, aby sa zabezpečilo správne zoradenie a výpis.
- Vizualizácia prebieha v renderovacej funkcii `render_joint_pmf()`, ktorá zobrazuje aj 2D aj 3D vizualizáciu podľa voľby užívateľa

---


## `model_conditional_mean()`

**Popis:**  
Funkcia modeluje **podmienenú strednú hodnotu E[Y|X]** pre spojitú odozvu `Y` vzhľadom na prediktor `X` pomocou zvolenej regresnej metódy. Používa sa na analýzu vzťahu medzi dvoma spojitými premennými.

---

**Parametre:**

| Názov              | Typ            | Popis |
|--------------------|----------------|-------|
| `data`             | `data.frame`   | Vstupný dataset obsahujúci odozvu a prediktor |
| `selected_variables` | `character[2]` | Mená stĺpcov – prvý je odozva (Y), druhý prediktor (X) |
| `mean_method`      | `character`    | Metóda modelovania: `"linear"`, `"poly"`, `"loess"`, `"gam"`, `"spline"`, `"exp"` |
| `poly_mean_degree` | `integer` (voliteľné) | Stupeň polynómu, ak je metóda `"poly"` |
| `specific_x`       | `numeric` (voliteľné) | Hodnota X pre výpočet špecifickej E[Y|X] (inak medián) |

---

**Podporované metódy (`mean_method`):**

- `"linear"` – Lineárna regresia
- `"poly"` – Polynomiálna regresia (vyžaduje `poly_mean_degree`)
- `"exp"` – Exponenciálna funkcia pomocou `nls()`
- `"spline"` – B-spline bázy s `df = 5`
- `"loess"` – Lokálne vyhladzovanie (LOESS)
- `"gam"` – Generalized Additive Model (spline smoothing)

---

**Výstup:**

Funkcia vracia `list` s týmito prvkami:

| Názov               | Popis |
|---------------------|-------|
| `conditional_mean`  | Dátový rámec s predikciami E[Y|X] pre 200 bodov |
| `specific_x`        | Konkrétna hodnota X, pre ktorú sa počítala E[Y|X] |
| `specific_mean`     | Hodnota podmienenej strednej hodnoty pre `specific_x` |
| `r_squared`         | Koeficient determinácie R² |
| `summary`           | `gt` tabuľka sumarizujúca metódu, bázu, hyperparametre a koeficienty |

---

**Poznámky:**

- Parametre modelu (napr. `β` koeficienty) sú súčasťou výstupnej tabuľky pre metódy `"linear"`, `"poly"`, `"spline"` a `"exp"`.
- `specific_x` je voliteľný, predvolene sa nastaví na medián prediktora.
- Funkcia nezahŕňa vizualizáciu — výstupné objekty sú vizualizované v renderovacej funkcii `combine_conditional_models()`.


## `model_conditional_quantiles()`

**Popis:**  
Funkcia modeluje **podmienené kvantilové funkcie** `Q_τ(Y|X)` pre spojitú odozvu `Y` vzhľadom na prediktor `X`. Používa kvantilovú regresiu (`quantreg::rq`) pre zvolené metódy a kvantily.

---

**Parametre:**

| Názov              | Typ              | Popis |
|--------------------|------------------|-------|
| `data`             | `data.frame`     | Vstupný dataset obsahujúci premenné `Y` a `X` |
| `selected_variables` | `character[2]`   | Mená stĺpcov – prvý je odozva (Y), druhý prediktor (X) |
| `quantile_method`  | `character`      | Metóda regresie: `"linear"`, `"poly"`, `"spline"` |
| `poly_quant_degree`| `integer` (voliteľné) | Stupeň polynómu pre `"poly"` metódu |
| `quantiles`        | `numeric vector` | Kvantily (napr. `c(0.25, 0.5, 0.75)`) |
| `specific_x`       | `numeric` (voliteľné) | Hodnota X pre výpočet konkrétneho kvantilu (τ) |

---

**Podporované metódy (`quantile_method`):**

- `"linear"` – Lineárna kvantilová regresia
- `"poly"` – Polynomiálna kvantilová regresia (vyžaduje `poly_quant_degree`)
- `"spline"` – Kvantilová regresia s B-spline bázou (s `df = 4`)

---

**Výstup:**

Funkcia vracia `list` s týmito prvkami:

| Názov                   | Popis |
|-------------------------|-------|
| `conditional_quantiles` | Dátový rámec s predikovanými kvantilmi pre každý τ na mriežke X |
| `specific_x`            | Hodnota X, pre ktorú sa vypočítali hodnoty kvantilov |
| `specific_quantiles`    | Mapa kvantilov τ → hodnota kvantilu Q_τ(Y|X = x) |
| `summaries`             | Zoznam `gt` tabuliek s parametrami a metrikami pre každý τ |

---

**Zahrnuté výstupné metriky pre každý kvantilový model:**

- Kvantil τ (napr. 0.5)
- Použitá metóda a báza (lineárna, spline, poly)
- Parametre regresie `β` (odhad ± chyba, ak dostupná)
- Hodnoty: Null deviance, Residual deviance, AIC

---

**Poznámky:**

- Ak nie je zadaný `specific_x`, hodnoty kvantilov sa vypočítajú pre medián prediktora.
- Funkcia nezahŕňa vizualizáciu — výstupné objekty sú vizualizované v renderovacej funkcii `combine_conditional_models()`.


## `model_discrete_predictor()`

**Popis:**  
Funkcia modeluje **podmienenú strednú hodnotu** \( E[Y \mid X = x] \), kde prediktor `X` je **diskrétny** (kategorický). Na základe typu modelu odhadne priemerné hodnoty odozvy pre jednotlivé kategórie.

---

**Parametre:**

| Názov               | Typ            | Popis |
|---------------------|----------------|-------|
| `data`              | `data.frame`   | Dataset obsahujúci premenné `Y` (response) a `X` (predictor) |
| `selected_variables`| `character[2]` | Vektor mien stĺpcov: prvý pre odozvu (Y), druhý pre prediktor (X) |
| `discrete_model_type` | `character`  | Typ modelu: `"lm"` (lineárny) alebo `"glm_log"` (GLM s log-linkom) |

---

**Podporované modely (`discrete_model_type`):**

- `"lm"` – klasická lineárna regresia (identity link)
- `"glm_log"` – GLM s logaritmickou väzbou (log link)

---

**Výstup:**

Funkcia vracia `list` s týmito komponentmi:

| Názov        | Popis |
|--------------|-------|
| `model`      | Fittovaný model (`lm` alebo `glm`) |
| `r_squared`  | R-squared alebo pseudo R² pre log-link |
| `plot`       | `ggplot` boxplot s priemermi a chybnými úsekmi pre každú kategóriu |
| `summary`    | `gt` tabuľka s typom modelu, väzbovou funkciou a metrikami |

---

**Vizualizácia:**

- Boxplot hodnoty `Y` pre každú kategóriu `X`
- Modré body: odhady priemerov pre každú kategóriu
- Vertikálne úsečky: smerodajné odchýlky
- Popis grafu zodpovedá typu použitého modelu

---

**Poznámky:**

- Ak `X` nie je faktor, funkcia ho automaticky konvertuje.
- Hodnota `R-squared` pre `"glm_log"` je vypočítaná ako \( 1 - \text{deviance} / \text{null deviance} \).
- Výstupná `summary` tabuľka obsahuje aj odhady parametrov \(\beta\) so štandardnými chybami, ak sú dostupné.


## `classification_model()`

**Popis:**  
Funkcia slúži na **modelovanie klasifikačných metód** s diskrétnou odozvou (`response_name`) a jedným alebo dvoma prediktormi (`predictor_names`). Podporuje rôzne metódy vrátane logistickej regresie, LDA, QDA a KNN.

---

**Parametre:**

| Názov             | Typ            | Popis |
|-------------------|----------------|-------|
| `data`            | `data.frame`   | Vstupné dáta |
| `response_name`   | `character`    | Názov stĺpca s diskrétnou odozvou |
| `predictor_names` | `character`    | Meno jedného alebo dvoch prediktorov |
| `method`          | `character`    | Metóda klasifikácie: `"logistic"`, `"lda"`, `"qda"`, `"knn"` |
| `k`               | `integer` alebo `NULL` | Počet susedov pre KNN (ak `NULL`, optimalizuje sa automaticky) |

---

**Podporované metódy (`method`):**

- `"logistic"` – binárna alebo multinomická logistická regresia
- `"lda"` – lineárna diskriminačná analýza
- `"qda"` – kvadratická diskriminačná analýza
- `"knn"` – klasifikácia metódou k najbližších susedov (KNN)

---

**Výstup:**

Funkcia vracia `list` s nasledovnými komponentmi:

| Komponent           | Popis |
|---------------------|-------|
| `model`             | Trénovaný klasifikačný model |
| `predictions`       | Vektor predikovaných tried |
| `accuracy`          | Celková presnosť klasifikácie |
| `confusion_matrix`  | Tabuľka zámien (confusion matrix) |
| `summary_gt`        | `gt` tabuľka s modelovými parametrami a metrikami |
| `decision_plot`     | Vizualizácia rozhodovacích hraníc (ak počet prediktorov ≤ 2) |

---

**Validácie a predspracovanie:**

- Odozva `response` musí byť diskrétna (faktor alebo celočíselná s <10 unikátnymi hodnotami).
- Prediktory sú automaticky konvertované na faktory, ak obsahujú ≤10 unikátnych hodnôt.
- Pre `qda` sa triedy s menej než 4 pozorovaniami odstránia (s upozornením).

---

**Vizualizácia:**

- Ak je 1 prediktor: volá sa funkcia `plot_classification_1D_combined()` -> 1D graf s rozhodovacími prahmi a podmienenými pravdepodobnosťami.
- Ak sú 2 prediktory: volá sa funkcia `plot_decision_boundary()` -> 2D rozhodovacie hranice na mriežke prediktorového priestoru.

---

**Sumarizačná tabuľka (`summary_gt`) obsahuje:**

- Typ použitého modelu
- Presnosť klasifikácie
- Prípadne odhady parametrov β s ich štandardnými chybami
- Pri QDA/LDA: priemery a smerodajné odchýlky na triedu a prediktor

---

**Poznámky:**

- Pre binárnu odozvu v `"logistic"` modeli sa ako prah používa 0.5.
- Pre `"knn"` sa automaticky optimalizuje parameter `k` od 1 do 20, ak nie je zadaný.
- Multinomická logistická regresia využíva `nnet::multinom()`.



## `model_conditional_continuous_densities()`

**Popis:**  
Funkcia **modeluje podmienené hustoty pravdepodobnosti spojitej odozvy vzhľadom na daný prediktor**. Podporuje ako diskrétne, tak aj spojité prediktory.

---

**Vstupné parametre:**

| Názov                | Typ               | Popis |
|----------------------|-------------------|-------|
| `df`                 | `data.frame`      | Vstupný dátový rámec s atribútmi `"response_var"` a `"predictor_var"` |
| `n_breaks`           | `integer`         | Počet sekcií (rezov) prediktora pre podmienené hustoty |
| `density_scaling`    | `numeric`         | Škálovací koeficient hustôt pre lepšiu vizualizáciu |
| `mean_curve`         | `logical`         | Či sa má pridať podmienená stredná hodnota |
| `quantiles`          | `numeric` vector  | Zoznam požadovaných kvantilov (napr. `c(0.25, 0.5, 0.75)`) |
| `mean_poly_degree`   | `integer`         | Stupeň polynómu pre strednú hodnotu |
| `quantile_poly_degree` | `integer`       | Stupeň polynómu pre kvantilové funkcie |
| `normal_density`     | `logical`         | Či zahrnúť normálnu hustotu do vizualizácie |
| `kernel_density`     | `logical`         | Či zahrnúť KDE (jadrový odhad hustoty) do vizualizácie |
| `bw_scale`           | `numeric` alebo `NULL` | Miera škálovania rozsahu vyhladzovania pre KDE |

---

**Spracovanie:**

- Funkcia najprv zistí, či je prediktor diskrétny alebo spojitý (pomocou `identify_variables()`, čo je funkcia na identifikáciu premenných).
- Pri **diskrétnych prediktoroch**:
  - Hustoty sa modelujú osobitne pre každú kategóriu.
  - Hustoty (KDE a/alebo normálne) sú škálované podľa ich výšky a počtu pozorovaní.
- Pri **spojitých prediktoroch**:
  - Modeluje sa buď pomocou neparametrického `kde2d` alebo parametrického viacrozmerného normálneho rozdelenia.
  - V každom `n_breaks` bode sa počíta podmienená hustota odozvy `Y | X = x`.

---

**Výstup:**

Funkcia vracia `list` s nasledujúcimi komponentmi:

| Komponent             | Popis |
|-----------------------|-------|
| `df`                  | Upravený dátový rámec s číselným prediktorom |
| `density_data`        | Zoznam dátových rámcov pre každú hustotu (`x`, `y`, `section`, `type`) |
| `breaks`              | Hodnoty prediktora, v ktorých sa počítali podmienené hustoty |
| `x_seq`               | Sekvencia hodnôt prediktora pre vizualizáciu |
| `mean_curve_data`     | Dátový rámec pre podmienenú strednú hodnotu (ak `mean_curve = TRUE`) |
| `quantile_data`       | Zoznam dátových rámcov pre každú kvantilovú krivku |
| `summary_table`       | Tabuľka sumarizujúca počet, priemer, SD, min a max odozvy v okolí každého delenia |
| `meta`                | Metadáta: mená premenných, typ prediktora, štatistiky (`mu`, `sd`, korelácia, epsilon, h_scaled`) |

---

**Podporované hustoty:**

- **KDE (jadrový odhad)** – pomocou `density()` a `approx()`
- **Normálna hustota** – pomocou `dnorm()` alebo viacrozmerného `dmvnorm()`

---

**Poznámky:**

- Hustoty sú škálované pomocou `fade_factor` (dočasná hustota slúžiaca na tlmenie) pre hladšie zobrazenie.
- `n_breaks` pre diskrétne premenné sa automaticky skracuje, ak je menej kategórií.
- Funkcia zahŕňa rozsiahle kontroly validnosti hustôt, `y_seq` a vyhladzovania.
- Výpočty hustôt sú doplnené aj o možnosť modelovať **podmienenú strednú hodnotu** a **kvantilové funkcie**.

---

**Vizualizácia:**

- Výstup tejto funkcie sa renderuje samostatne vo funkcii `render_conditional_continuous_densities()`.
- Každá hustota má svoj typ (`"KDE"` alebo `"normal"`), čo umožňuje farebne alebo štýlovo ich odlíšiť.


## `model_conditional_discrete_densities()`

**Popis:**  
Funkcia **modeluje podmienenú pravdepodobnostnú funkciu diskrétnej odozvy vzhľadom na diskrétny alebo spojitý prediktor** pomocou Bayesovho pravidla. Podporuje viacero odhadových metód: empirický výpočet (pri diskrétnom prediktore), jadrový odhad hustoty (KDE) a parametrický normálny (Gaussov) model.

---

**Vstupné parametre:**

| Názov              | Typ              | Popis |
|--------------------|------------------|-------|
| `df`               | `data.frame`     | Dátový rámec s atribútmi `"response_var"` a `"predictor_var"` |
| `n_breaks`         | `integer`        | Počet sekcií pre spojitý prediktor, kde sa budú počítať podmienené pravdepodobnosti |
| `density_scaling`  | `numeric`        | Činiteľ škálovania pravdepodobností pre vizualizáciu |
| `normal_density`   | `logical`        | Či sa má použiť Gaussov model pre odhad hustoty \( f_{C \mid D}(c \mid d) \) |
| `kernel_density`   | `logical`        | Či sa má použiť neparametrický jadrový odhad hustoty \( f_{C \mid D}(c \mid d) \) |

---

**Spracovanie:**

- Funkcia najprv konvertuje odozvu na číselné hodnoty (pre interné výpočty) a uchová pôvodné kategórie.
- **Ak je prediktor diskrétny**:
  - Vypočíta podmienené pravdepodobnosti pre každú kombináciu kategórie prediktora a odozvy.
  - Pravdepodobnosti sa označia ako `"Empirical"`.
- **Ak je prediktor spojitý**:
  - Vygenerujú sa `n_breaks` sekcie (stredy intervalov), v ktorých sa počítajú podmienené pravdepodobnosti.
  - Pre každú úroveň odozvy sa odhadne hustota prediktora podmienená touto úrovňou (`KDE` alebo `Normal`), aj marginálna hustota prediktora.
  - Výsledná pravdepodobnosť sa počíta podľa Bayesovho vzorca:  
    \[
    \Pr(D = d \mid C = c) = \frac{\Pr(D = d) \cdot f_{C \mid D}(c \mid d)}{f_C(c)}
    \]

---

**Výstup:**

Funkcia vracia `list` s nasledovnými komponentmi:

| Komponent             | Popis |
|-----------------------|-------|
| `df`                  | Upravený dátový rámec vrátane `predictor_numeric` |
| `density_df`          | Výsledné podmienené pravdepodobnosti (obsahuje `x_center`, `prob`, `Category`, `method`, `section`) |
| `response_levels`     | Úrovne kategórií odozvy |
| `predictor_is_discrete` | Boolean hodnota indikujúca typ prediktora |
| `response_name`       | Názov odozvy |
| `predictor_name`      | Názov prediktora |

---

**Podporované metódy výpočtu:**

| Metóda      | Popis |
|-------------|-------|
| `Empirical` | Výpočet frekvenčným pomerom (iba pre diskrétny prediktor) |
| `KDE`       | Neparametrický jadrový odhad hustoty |
| `Normal`    | Parametrický Gaussov model hustoty |

---

**Vizualizácia:**

- Vizualizáciu zabezpečuje funkcia `render_conditional_discrete_densities()`, ktorá:
  - zobrazuje podmienené pravdepodobnosti ako horizontálne segmenty škálované podľa výšky pravdepodobnosti,
  - používa rôzne **tvary a čiary** podľa typu metódy (`linetype`, `shape`),
  - voliteľne spája hodnoty podľa úrovní odozvy pomocou `geom_path()` (ak `ordinal = TRUE`).

---

**Poznámky:**

- Výstupná tabuľka s pravdepodobnosťami sa sumarizuje a zobrazuje pomocou `gt`.
- Odozva je považovaná za **diskrétnu s usporiadanými kategóriami** (ak sa používa `ordinal = TRUE`).
- Pri malej vzorke pre konkrétnu kategóriu alebo extrémnych hustotách sa môžu niektoré sekcie vynechať.

