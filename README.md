# Phan Tich Nha Dat TP.HCM

He thong phan tich va du doan gia bat dong san TP. Ho Chi Minh xay dung bang ngon ngu R.
Du an thu thap du lieu tu nhieu nguon (Cho Tot API va Alonhadat HTML), gop ve mot schema chung, thuc hien phan tich kham pha (EDA), huan luyen mo hinh hoc may (Linear Regression, Random Forest, XGBoost, Ensemble), phan cum K-Means va trien khai ket qua qua Shiny dashboard tuong tac.

Mon: Lap Trinh R — Do an Cuoi ky — HCMUTE

---

## Muc luc

1. [Yeu cau he thong](#yeu-cau-he-thong)
2. [Cau truc thu muc](#cau-truc-thu-muc)
3. [Khoi dong tu A den Z](#khoi-dong-tu-a-den-z)
4. [Thu thap them du lieu tu nguon khac](#thu-thap-them-du-lieu-tu-nguon-khac)
5. [Train lai mo hinh](#train-lai-mo-hinh)
6. [Chi tiet tung file](#chi-tiet-tung-file)
7. [Cong nghe su dung](#cong-nghe-su-dung)
8. [Mo ta cac tab dashboard](#mo-ta-cac-tab-dashboard)

---

## Yeu cau he thong

| Thanh phan | Phien ban toi thieu |
|---|---|
| R | 4.2.0 tro len |
| RStudio (tuy chon) | 2023.x tro len |
| RAM | 4 GB (khuyen nghi 8 GB) |
| Ket noi Internet | Can thiet khi scrape du lieu |

### Cac package R can thiet

**Pipeline & scraping:**
```r
install.packages(c(
  "httr", "jsonlite", "dplyr", "purrr", "furrr", "future",
  "readr", "lubridate", "DBI", "RSQLite", "rvest", "xml2", "stringr", "tibble"
))
```

**Feature engineering & EDA:**
```r
install.packages(c("dplyr", "readr", "lubridate", "ggplot2"))
```

**Huan luyen mo hinh:**
```r
install.packages(c(
  "readr", "dplyr", "lubridate",
  "randomForest", "xgboost", "Matrix", "ggplot2", "tibble"
))
```

**Shiny dashboard:**
```r
install.packages(c(
  "shiny", "dplyr", "readr", "lubridate",
  "ggplot2", "plotly", "DT", "randomForest", "leaflet"
))
```

---

## Cau truc thu muc

```
phantichnhadathcm/
|
|-- app.R                        # Shiny dashboard chinh (UI + Server)
|-- run_app.R                    # Script khoi dong app nhanh
|
|-- scripts/
|   |-- run_pipeline.R           # Chay toan bo pipeline tu A den Z
|   |-- chotot_scraper_v3.R      # Thu thap du lieu tu Cho Tot API
|   |-- alonhadat_scraper.R      # Thu thap du lieu bo sung tu Alonhadat
|   |-- merge_sources.R          # Gom raw data nhieu nguon ve mot schema
|   |-- feature_engineering.R    # Tao features cho ML
|   |-- eda_analysis.R           # Phan tich kham pha, xuat bieu do
|   |-- train_models.R           # Huan luyen LR / RF / XGBoost / Ensemble / KMeans
|   |-- update_data.R            # Cap nhat nhanh data, khong retrain
|   |-- auto_update.R            # Cap nhat data va retrain co dieu kien
|   |-- validate_project.R       # Kiem tra nhanh data, model, app prediction
|
|-- data/
|   |-- hcmc_bds.sqlite          # Co so du lieu SQLite (chinh)
|   |-- hcmc_bds_raw.csv         # Du lieu tho sau scrape
|   |-- hcmc_bds_combined_raw.csv # Du lieu tho da gom nhieu nguon
|   |-- hcmc_bds_featured.csv    # Du lieu sau feature engineering
|   |-- sources/
|       |-- alonhadat_raw.csv    # Raw data rieng tung nguon
|
|-- models/
|   |-- price_models_sale.rds    # Mo hinh du doan gia ban
|   |-- price_models_rent.rds    # Mo hinh du doan gia thue
|   |-- rf_importance_sale.csv   # Feature importance (ban)
|   |-- rf_importance_rent.csv   # Feature importance (thue)
|   |-- model_metrics.csv        # Ket qua danh gia mo hinh
|   |-- kmeans_area_price.rds    # Mo hinh K-Means clustering
|   |-- kmeans_area_price.csv    # Ket qua phan cum
|
|-- plots/                       # Bieu do EDA va mo hinh (.png)
|-- www/
|   |-- hcmute-logo.png          # Logo hien thi trong dashboard
|-- docs/
|   |-- KeHoach_DoAn_BDS_HCMC.docx
|   |-- diagrams/
|-- R_libs/                      # Thu vien R local (tuy chon)
```

---

## Khoi dong tu A den Z

### Buoc 1: Tai du an ve may

Sao chep thu muc du an hoac clone git repository ve may tinh.

```bash
cd /duong/dan/den/thu/muc
ls   # Kiem tra co thay app.R la du
```

### Buoc 2: Mo R va cai dat thu vien

Mo RStudio hoac chay R trong terminal, sau do cai tat ca package can thiet:

```r
install.packages(c(
  "httr", "jsonlite", "dplyr", "purrr", "furrr", "future",
  "readr", "lubridate", "DBI", "RSQLite",
  "randomForest", "xgboost", "Matrix",
  "ggplot2", "plotly", "tibble",
  "shiny", "DT", "leaflet"
))
```

> Neu may ban chay cham, co the luu thu vien vao thu muc `R_libs/` de su dung lai:
> ```r
> dir.create("R_libs")
> install.packages("ten_package", lib = "R_libs")
> ```
> Script se tu dong them duong dan nay vao `.libPaths()` khi chay.

### Buoc 3: Thu thap du lieu

> **Bo qua buoc nay neu thu muc `data/` da co file `hcmc_bds_featured.csv`.**

```bash
# Tu terminal, dung thu muc goc cua du an
Rscript scripts/chotot_scraper_v3.R
```

Hoac trong R:

```r
setwd("/duong/dan/den/du/an")
source("scripts/chotot_scraper_v3.R")
run_scrape()
```

Ket qua:
- `data/hcmc_bds.sqlite` — co so du lieu SQLite luu tin dang.
- `data/hcmc_bds_raw.csv` — toan bo du lieu tho.

**Dieu chinh so luong trang scrape (mac dinh 50 trang/loai):**

```bash
CHOTOT_MAX_PAGES=100 Rscript scripts/chotot_scraper_v3.R
```

Hoac trong R:

```r
run_scrape(max_pages = 100)
```

### Buoc 4: Feature engineering

```bash
Rscript scripts/feature_engineering.R
```

Ket qua: `data/hcmc_bds_featured.csv` chua cac features da xu ly dung cho ML.

### Buoc 5: Phan tich kham pha (tuy chon)

```bash
Rscript scripts/eda_analysis.R
```

Ket qua: cac bieu do `.png` va `plots/eda_summary.csv`.

### Buoc 6: Huan luyen mo hinh

```bash
Rscript scripts/train_models.R
```

Ket qua:
- `models/price_models_sale.rds`
- `models/price_models_rent.rds`
- `models/model_metrics.csv`
- `models/rf_importance_sale.csv`
- `models/rf_importance_rent.csv`
- `models/kmeans_area_price.rds`
- `models/kmeans_area_price.csv`

### Buoc 7: Khoi dong dashboard

```bash
Rscript run_app.R
```

Hoac trong R/RStudio:

```r
shiny::runApp(".", host = "127.0.0.1", port = 3838)
```

Mo trinh duyet tai: `http://127.0.0.1:3838`

---

### Chay toan bo pipeline mot lenh (A den Z)

```bash
Rscript scripts/run_pipeline.R
```

Script nay chay tuan tu: scrape -> feature engineering -> EDA -> train models.
Sau khi hoan tat, chay tiep:

```bash
Rscript run_app.R
```

---

## Thu thap them du lieu tu nguon khac

He thong hien co 2 nguon:

| Nguon | Script | Output rieng |
|---|---|---|
| Cho Tot | `scripts/chotot_scraper_v3.R` | `data/hcmc_bds_raw.csv` |
| Alonhadat | `scripts/alonhadat_scraper.R` | `data/sources/alonhadat_raw.csv` |

Sau khi scrape tung nguon, chay `scripts/merge_sources.R` de gom tat ca ve:

```bash
Rscript scripts/merge_sources.R
```

Ket qua gom chung nam o `data/hcmc_bds_combined_raw.csv`. File feature engineering se uu tien doc file combined nay truoc, nen model luon train tren du lieu da gop nhieu nguon.

### Crawl them Alonhadat

```bash
# Mac dinh 6 trang moi nhom
Rscript scripts/alonhadat_scraper.R

# Tang so trang neu can nhieu data hon
ALONHADAT_MAX_PAGES=10 Rscript scripts/alonhadat_scraper.R
```

### Schema chuan cho moi nguon

Neu them scraper moi (vd Batdongsan, Muaban, Nha Tot khi truy cap duoc), file scraper nen xuat CSV vao `data/sources/` voi cac cot sau:

| Ten cot | Kieu du lieu | Mo ta |
|---|---|---|
| `source` | character | Ten nguon, vd `alonhadat` |
| `source_group` | character | Nhom URL/danh muc trong nguon |
| `source_id` | character | ID duy nhat theo nguon |
| `ad_id` | character | ID tin dang, duy nhat |
| `title` | character | Tieu de tin |
| `price` | numeric | Gia (don vi: VND) |
| `area` | numeric | Dien tich (m2) |
| `rooms` | numeric | So phong ngu |
| `ward` | character | Ten phuong/xa |
| `district_name` | character | Ten quan/huyen |
| `category_name` | character | Loai BDS (vd: "Can ho/Chung cu") |
| `category_id` | character | Ma loai (xem bang ben duoi) |
| `lat` | numeric | Vi do (co the NA) |
| `lon` | numeric | Kinh do (co the NA) |
| `ad_url` | character | URL tin dang |
| `source_url` | character | URL trang danh sach da crawl |
| `posted_at` | character | Thoi gian dang (ISO 8601) |
| `is_rent` | logical | TRUE = cho thue, FALSE = ban |

**Bang ma category_id:**

| category_id | category_name |
|---|---|
| `1010` | Can ho/Chung cu |
| `1020` | Nha o |
| `1030` | Van phong/Mat bang |
| `1040` | Dat |
| `1050` | Phong tro |

### Cap nhat nhanh du lieu moi

Neu chi muon lay them tin moi va cap nhat dashboard, khong can train lai model moi lan:

```bash
Rscript scripts/update_data.R
```

Script nay se scrape nhanh Cho Tot, scrape nhanh Alonhadat, merge sources va tao lai `data/hcmc_bds_featured.csv`.

Neu muon cap nhat data va chi retrain khi that su can:

```bash
Rscript scripts/auto_update.R
```

Mac dinh `auto_update.R` se retrain khi model qua 7 ngay hoac so dong data moi tang tu 12% tro len. Co the dieu chinh:

```bash
RETRAIN_MIN_NEW_RATIO=0.2 RETRAIN_MAX_MODEL_AGE_DAYS=14 Rscript scripts/auto_update.R
FORCE_RETRAIN=1 Rscript scripts/auto_update.R
```

Trong dashboard cung co nut **Lam moi du lieu** tren topbar. Nut nay goi `scripts/auto_update.R`, sau do dashboard nap lai data/model moi.

Kiem tra nhanh sau khi cap nhat:

```bash
Rscript scripts/validate_project.R
```

---

## Train lai mo hinh

### Khi nao can train lai?

- Da thu thap them du lieu moi du nhieu, vi du tang tren 10-20%.
- Model da cu, vi du qua 7 ngay trong boi canh thi truong thay doi.
- Muon tang do chinh xac bang cach tang `ntree` cua Random Forest hoac `nrounds` cua XGBoost.
- Muon them features moi vao mo hinh.

### Chay train lai

```bash
Rscript scripts/feature_engineering.R   # Tao lai features
Rscript scripts/train_models.R          # Train lai tat ca mo hinh
```

### Dieu chinh hyperparameter

Mo file `scripts/train_models.R` va chinh cac tham so sau:

**Random Forest** (dong ~147):
```r
rf_model <- randomForest(
  formula,
  data = train,
  ntree = 300,       # Tang len 500 de chinh xac hon, nhung chay cham hon
  importance = TRUE
)
```

**XGBoost** (dong ~158):
```r
xgb_model <- xgboost(
  x = x_train,
  y = train$log_price,
  nrounds = 250,         # Tang len 400-500 de chinh xac hon
  learning_rate = 0.05,  # Giam xuong 0.03 neu nrounds cao
  max_depth = 6,         # Tang len 8 neu du lieu phuc tap
  subsample = 0.8,
  colsample_bytree = 0.8,
  ...
)
```

**Sau khi train xong**, chay validate de dam bao app doc duoc model va du doan duoc ca ban/thue:

```bash
Rscript scripts/validate_project.R
```

Khoi dong lai app de phan Hieu nang mo hinh va Bieu do Feature Importance cap nhat theo mo hinh moi:

```bash
Rscript run_app.R
```

---

## Chi tiet tung file

### `app.R`

**Cong nghe:** R, Shiny, plotly, leaflet, DT, ggplot2, randomForest

File duy nhat chua toan bo Shiny application (UI + Server). App co nut **Lam moi du lieu** tren topbar, goi `scripts/auto_update.R` de cap nhat data va retrain co dieu kien. Duoc to chuc theo cac thanh phan:

| Thanh phan | Dong | Chuc nang |
|---|---|---|
| Package check | 9-22 | Kiem tra package truoc khi load, bao loi ro rang neu thieu |
| Data loading | 34-70 | Doc CSV/fallback du lieu, chuan hoa kieu du lieu |
| Helper functions | 72-176 | `format_vnd`, `predict_price`, `price_color`, `kpi_card`, `app_panel`, `interactive_chart` |
| CSS | 178-457 | Toan bo CSS noi trang: CSS variables, layout, sidebar, topbar, KPI cards, responsive breakpoints |
| UI definition | 459-650 | Dinh nghia giao dien: sidebar nav, tabsetPanel an, 7 tab chinh |
| Server: data | 655-690 | `reactiveTimer` 15 phut, `listings()`, `metrics()`, `filtered()`, `map_filtered()` |
| Server: outputs | 691-990 | Render KPI, bieu do, ban do, bang du lieu, du doan, clustering |

**Cac output chinh trong Server:**

| Output ID | Loai | Noi dung |
|---|---|---|
| `kpi_cards` | renderUI | 4 the KPI: so tin, gia trung vi, quan/huyen, mo hinh tot nhat |
| `listing_map` | renderLeaflet | Ban do tuong tac voi CircleMarkers phan cum |
| `data_table` | renderDT | Bang du lieu co link clickable, loc theo cot |
| `importance_plot` | renderPlotly | Feature importance RF (tu dong chon sale/rent) |
| `prediction_text` | renderText | Gia du doan dang text |
| `cluster_plot` | renderPlotly | Bieu do K-Means phan cum khu vuc |

---

### `scripts/chotot_scraper_v3.R`

**Cong nghe:** httr, jsonlite, furrr, future, DBI, RSQLite

Thu thap du lieu tu Cho Tot public API (`gateway.chotot.com/v1/public/ad-listing`).

| Thanh phan | Chuc nang |
|---|---|
| `CFG` | Cau hinh: so trang, so worker, delay giua request, duong dan xuat |
| `DISTRICTS` | Ban do district_id -> ten quan (19 quan TP.HCM) |
| `CATEGORIES` | Ban do category_id -> ten loai BDS (5 loai) |
| `UA_POOL` | Xoay vong User-Agent de tranh bi block |
| `price_ranges` | Khoang gia hop le theo loai BDS de loc outlier |
| `init_db()` | Tao bang `listings` trong SQLite voi PRIMARY KEY la `ad_id` |
| `fetch_page()` | Goi 1 trang API, parse JSON, tra ve tibble |
| `clean_listings()` | Loc du lieu hop le: gia > 0, dien tich 5-5000m2, trong khoang gia |
| `upsert_listings()` | Ghi vao SQLite dung `INSERT OR IGNORE` de chong trung |
| `run_scrape()` | Ham chinh: tao combos (district x category x page), chay parallel voi furrr |
| `refresh_data()` | Scrape nhanh chi 3 trang moi de cap nhat tin moi |

**Bien moi truong dieu chinh:**

```bash
CHOTOT_MAX_PAGES=100    # So trang moi nhan/category (mac dinh: 50)
CHOTOT_WORKERS=8         # So luong worker parallel (mac dinh: 4)
CHOTOT_USE_AREA_FILTER=1 # Loc theo tung quan (chay cham hon, du lieu chinh xac hon)
```

---

### `scripts/feature_engineering.R`

**Cong nghe:** dplyr, readr, lubridate

Xu ly va tao them features tu du lieu tho cho phu hop voi ML.

| Feature tao ra | Mo ta |
|---|---|
| `price_m` | Gia theo trieu VND |
| `price_per_m2` | Gia/m2 (VND) |
| `log_price` | log(1 + price) — bien muc tieu cho mo hinh |
| `log_area` | log(1 + area) |
| `log_price_per_m2` | log(1 + price_per_m2) |
| `distance_to_center` | Khoang cach Haversine toi trung tam (quan 1, 10.7758N 106.7009E) tinh bang km |
| `ward_median_price` | Gia trung vi theo phuong/xa |
| `ward_price_encoded` | log(1 + ward_median_price) — target encoding |
| `listing_age_days` | So ngay ke tu khi dang tin |
| `posted_hour` | Gio dang tin |
| `posted_wday` | Thu trong tuan dang tin |
| `is_weekend_post` | TRUE neu dang vao cuoi tuan |
| `is_rent` | TRUE neu la tin cho thue |
| `transaction_type` | Ban / Cho thue |
| `price_segment` | Nhom tu 1-4 (Re / Trung binh / Cao / Cao cap) |

**Xu ly missing values:**
- `area` NA: thay bang median theo (district_name, category_name).
- `rooms` NA: thay bang mode theo category_name.
- `distance_to_center` NA: thay bang median toan bo.
- Toa do ngoai khu vuc TP.HCM se duoc dua ve NA de tranh marker lech ban do.
- `posted_at` bi thieu se fallback theo `scraped_at` neu co.

---

### `scripts/eda_analysis.R`

**Cong nghe:** dplyr, readr, lubridate, ggplot2

Phan tich kham pha du lieu, xuat 8 bieu do `.png` va 1 file CSV tom tat.

| File output | Noi dung |
|---|---|
| `01_log_price_distribution.png` | Histogram phan phoi log(gia) |
| `02_price_by_district_boxplot.png` | Boxplot gia theo tung quan |
| `03_area_vs_price.png` | Scatter plot dien tich vs gia, mau theo loai BDS |
| `04_top_districts_by_listing.png` | Top 10 quan nhieu tin nhat |
| `05_top_districts_by_price_m2.png` | Top 10 quan gia/m2 cao nhat |
| `06_listing_trend_by_day.png` | Xu huong so tin theo ngay |
| `07_geo_scatter_price.png` | Phan bo dia ly, mau theo log(gia) |
| `08_price_m2_by_category.png` | Boxplot gia/m2 theo loai BDS |
| `eda_summary.csv` | Bang tom tat theo quan va loai BDS |

---

### `scripts/train_models.R`

**Cong nghe:** randomForest, xgboost, Matrix, ggplot2, tibble

Huan luyen 4 mo hinh hoc may va 1 thuat toan phan cum tren 2 tap du lieu (ban va thue).

**Quy trinh huan luyen:**

```
Du lieu -> Chia train/test theo thoi gian 80/20 -> Target encoding tren train -> Huan luyen 4 mo hinh -> Chon best model -> Luu .rds + registry
```

| Mo hinh | Bien muc tieu | Features chinh |
|---|---|---|
| Linear Regression | log_price | area, rooms, district_name, category_name, posted_hour, distance_to_center, ward_price_encoded |
| Random Forest (ntree=300) | log_price | Nhu tren |
| XGBoost (nrounds=250) | log_price | Nhu tren (sparse matrix) |
| RF + XGBoost Ensemble | log_price | Trung binh pred cua RF va XGB |

**Cac diem ML quan trong:**
- Split uu tien theo thoi gian (`time_based`) de test bang cac tin moi hon, sat voi luc demo thuc te.
- `ward_price_encoded` duoc fit tren train set roi moi ap dung sang test, tranh data leakage.
- Moi segment `sale` / `rent` luu best model rieng trong `models/model_registry.csv`.
- App doc best model trong bundle `.rds`; neu sale tot nhat la ensemble thi app dung ensemble, khong con co dinh Random Forest.
- Model bundle luu factor levels, target encoding table va cot XGBoost de prediction on-web on dinh hon.

**Chi so danh gia mo hinh:**

| Chi so | Y nghia |
|---|---|
| RMSE | Can bac hai sai so trung binh (VND) — cang nho cang tot |
| MAE | Sai so tuyet doi trung binh (VND) |
| MAPE | Sai so tuong doi trung binh (%) — chi so de hieu nhat |
| R2 | He so xac dinh — cang gan 1 cang tot |

**K-Means clustering:**
- Nhom cac cum (district x category) theo: gia/m2 trung vi, dien tich trung vi, so tin.
- So cum k = min(4, so nhom co du lieu).
- Ket qua luu vao `models/kmeans_area_price.csv` de hien thi trong dashboard.

---

### `scripts/run_pipeline.R`

**Cong nghe:** base R (source)

Script tien loi chay toan bo pipeline theo thu tu:
1. Scrape du lieu Cho Tot.
2. Scrape du lieu Alonhadat.
3. Gop raw data nhieu nguon.
4. Feature engineering.
5. EDA.
6. Train models.

Su dung khi can chay lai pipeline day du tu dau:

```bash
Rscript scripts/run_pipeline.R
```

---

### `run_app.R`

**Cong nghe:** shiny

Khoi dong Shiny app tren localhost port 3838. Su dung khi khong muon go lenh day du trong R console.

```bash
Rscript run_app.R
```

---

## Cong nghe su dung

| Muc dich | Package / Cong nghe |
|---|---|
| HTTP requests | httr |
| Parse JSON API | jsonlite |
| Co so du lieu | DBI + RSQLite (SQLite) |
| Xu ly du lieu | dplyr, tidyr, purrr, furrr |
| Xu ly ngay gio | lubridate |
| Doc/ghi file | readr |
| Parallel processing | future, furrr |
| Truc quan hoa | ggplot2, plotly |
| Ban do tuong tac | leaflet |
| Bieu do thanh phan | plotly (via ggplotly) |
| Bang du lieu tuong tac | DT (DataTables) |
| Mo hinh hoc may | randomForest, xgboost, Matrix |
| Web dashboard | shiny |
| Typography | Inter (Google Fonts), JetBrains Mono |

---

## Mo ta cac tab dashboard

| Tab | Chuc nang |
|---|---|
| Tong quan | 4 KPI cards, bieu do so tin theo quan, co cau loai BDS, hieu nang mo hinh |
| Ban do du lieu | Ban do Leaflet tuong tac, mau marker theo gia, popup chi tiet, bo loc nguon/giao dich/quan/loai/gia/dien tich |
| Phan tich gia | Scatter dien tich vs gia, top quan theo gia/m2, boxplot gia theo loai, histogram log(gia), bo loc nguon/giao dich |
| Du doan gia | Form nhap thong tin BDS va loai giao dich, ket qua du doan bang Random Forest, bieu do feature importance |
| Phan cum khu vuc | Bubble chart K-Means phan cum quan theo gia/m2 va dien tich |
| Du lieu | Bang day du co tim kiem, bo loc nguon/giao dich/khu vuc/loai, link tin dang clickable |
| Ve do an | Mo ta pipeline va cong nghe su dung |

---

## Luu y

- Du lieu scrape tu Cho Tot/Alonhadat co the co tin het han (link 404) do trang chu da go tin. Day la hien tuong binh thuong, khong phai loi cua he thong.
- Dashboard doc du lieu da crawl va train gan nhat; day la du lieu cap nhat theo pipeline, khong phai realtime streaming.
- Khong nen tang `CHOTOT_MAX_PAGES` qua 200 de tranh bi block IP.
- Neu can du lieu co toa do (lat/lon) cho ban do, nen de `CHOTOT_USE_AREA_FILTER=1` de scrape theo tung quan, do chinh xac toa do se cao hon.
- Thu muc `R_libs/` (neu co) chua cac package da cai dat local, script se tu dong su dung.
