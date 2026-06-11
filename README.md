# Phan Tich Nha Dat TP.HCM

Du an R/Shiny phan tich, truc quan hoa va du doan gia bat dong san TP.HCM tu nhieu nguon du lieu: Cho Tot, Alonhadat, Luachonnhadat, Muaban, Mogi va Homedy.

## Cau Truc Thu Muc

```text
phantichnhadathcm/
|-- app.R
|-- chay_ung_dung.R
|-- README.md
|-- ung_dung/
|   |-- ham_ho_tro_ung_dung.R           # Ham xu ly data, model, thong ke, tro ly
|   |-- giao_dien_ung_dung.R            # Layout va input/output placeholder
|   `-- may_chu_ung_dung.R              # Reactive, observer va render output
|-- .local/
|   `-- thu_vien_r/                       # Thu vien R local, khong phai data
|-- data/
|   |-- main/
|   |   `-- du_lieu_chinh.csv           # DATA MAIN app/model dang dung
|   |-- interim/
|   |   `-- du_lieu_gop_nguon.csv
|   |-- raw/
|   |   |-- chotot/
|   |   |-- alonhadat/
|   |   |-- luachonnhadat/
|   |   |-- muaban/
|   |   |-- mogi/
|   |   `-- homedy/
|   |-- cache/
|   |   `-- cache_chotot.sqlite
|   `-- logs/
|-- scripts/
|   |-- config/       # Cau hinh path trung tam
|   |-- lib/          # Ham dung chung
|   |-- scrapers/     # Lay du lieu tu web/API
|   |-- importers/    # Chuan hoa CSV local ve schema chung
|   |-- processing/   # Merge sources, feature engineering
|   |-- models/       # Train model
|   |-- analysis/     # EDA/plots
|   |-- pipeline/     # Chay/cap nhat pipeline
|   `-- checks/       # Validation/smoke test
|-- models/
|-- plots/
|-- docs/
`-- www/
```

## Data

`data/main/du_lieu_chinh.csv` la file chinh app Shiny va model dang doc.

`data/interim/du_lieu_gop_nguon.csv` la file da gop tat ca nguon ve mot schema chung, truoc buoc tao feature.

`data/raw/<source>/` chua du lieu rieng tung nguon:

| Nguon | File chinh |
|---|---|
| Cho Tot | `data/raw/chotot/chotot_schema_chuan.csv` |
| Alonhadat | `data/raw/alonhadat/alonhadat_schema_chuan.csv` |
| Alonhadat local | `data/raw/alonhadat/alonhadat_local_schema_chuan.csv` |
| Luachonnhadat | `data/raw/luachonnhadat/luachonnhadat_schema_chuan.csv` |
| Muaban | `data/raw/muaban/muaban_schema_chuan.csv` |
| Mogi | `data/raw/mogi/mogi_schema_chuan.csv` |
| Homedy | `data/raw/homedy/homedy_schema_chuan.csv` |

`data/cache/` chi chua cache SQLite phuc vu scraper. `data/logs/` chua log update pipeline.


## Lenh Hay Dung

Chay app:

```bash
Rscript chay_ung_dung.R
```

## Dashboard

App Shiny hien co cac nhom chuc nang chinh:

| Tab | Noi dung |
|---|---|
| Tong quan | KPI thi truong, top khu vuc, co cau loai BDS, metrics model |
| Ban do du lieu | Leaflet marker cluster, loc nguon/giao dich/khu vuc/loai/gia/dien tich |
| Phan tich gia | Scatter, histogram, ECDF, heatmap khu vuc x loai BDS, sunburst nguon, xu huong thoi gian, correlation |
| Suy luan thong ke | Xac suat co dieu kien, CLT simulation, bootstrap CI, kiem dinh gia thuyet |
| Du doan gia | Form du doan bang model tot nhat va vung gia tham khao tu listing tuong dong |
| Danh gia model | Model card, actual vs predicted, residual, sai so theo khu vuc, so sanh metric |
| Phan cum khu vuc | K-Means theo gia/m2, dien tich trung vi va so tin |
| Du lieu | Bang data sach, data quality check, do phu nguon va toa do |
| Tro ly BDS | Tro ly local dung data/model trong project, khong goi API ngoai |

Kiem tra nhanh du an:

```bash
Rscript scripts/checks/kiem_tra_du_an.R
```

Chay pipeline day du:

```bash
Rscript scripts/pipeline/chay_pipeline.R
```

Cap nhat data nhanh, khong retrain:

```bash
Rscript scripts/pipeline/cap_nhat_du_lieu.R
```

Cap nhat data va retrain co dieu kien:

```bash
Rscript scripts/pipeline/tu_dong_cap_nhat.R
```

Chay tung buoc xu ly:

```bash
Rscript scripts/processing/gop_nguon_du_lieu.R
Rscript scripts/processing/tao_dac_trung.R
Rscript scripts/analysis/phan_tich_eda.R
Rscript scripts/models/huan_luyen_mo_hinh.R
```

## Path Trung Tam

Tat ca duong dan quan trong nam trong:

```text
scripts/config/duong_dan_du_an.R
```

Khi doi vi tri file data/script, uu tien sua file nay truoc thay vi hard-code nhieu noi.
