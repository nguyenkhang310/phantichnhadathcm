---
title: Phan Tich Nha Dat TP.HCM
sdk: docker
app_port: 3838
---

# Phan Tich Nha Dat TP.HCM

Du an R/Shiny phan tich, truc quan hoa va du doan gia bat dong san TP.HCM tu nhieu nguon du lieu: Cho Tot, Alonhadat, Luachonnhadat, Muaban, Mogi va Homedy.

## Cau Truc Thu Muc

```text
phantichnhadathcm/
|-- app.R
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
Rscript app.R
```

## Deploy Len Internet

Du an da co san cau hinh Docker, Render va Hugging Face Spaces:

```text
Dockerfile
.dockerignore
render.yaml
```

### Chay thu bang Docker tren may

Build image:

```bash
docker build -t phantichnhadathcm .
```

Chay app:

```bash
docker run --rm -p 3838:3838 -e BDS_APP_PORT=3838 phantichnhadathcm
```

Mo trinh duyet tai:

```text
http://localhost:3838
```

### Deploy bang Render

Project dung Git LFS cho cac file model `.rds` lon. Truoc khi push len GitHub, kiem tra Git LFS:

```bash
git lfs install
git lfs ls-files
```

1. Push toan bo project len GitHub.
2. Vao Render Dashboard.
3. Chon New > Blueprint.
4. Chon repo GitHub cua project.
5. Render se doc file `render.yaml` va tao web service `phantichnhadathcm`.
6. Bam Apply/Deploy.
7. Sau khi build xong, app se co URL dang `https://phantichnhadathcm.onrender.com`.

File `render.yaml` dang dung goi `free` de deploy thu mien phi. Vi project co model `.rds` lon, app co the cham, bi sleep khi khong co truy cap, hoac loi thieu RAM. Neu can chay on dinh hon, doi `plan: free` thanh `plan: starter`.

App doc cong chay theo thu tu uu tien:

```text
BDS_APP_PORT > PORT > 3838
```

Khi deploy tren Render, nen de Render tu cap bien `PORT`. Khi chay local co the dung `BDS_APP_PORT=3838`.

### Deploy bang Hugging Face Spaces

1. Vao https://huggingface.co/spaces
2. Chon Create new Space.
3. Dat Space name la `phantichnhadathcm`.
4. Chon SDK la `Docker`.
5. Chon visibility `Public`.
6. Tao Space.
7. Them remote Hugging Face vao repo local:

```bash
git remote add hf https://huggingface.co/spaces/<username>/phantichnhadathcm
```

8. Push code va file LFS len Space:

```bash
git push hf main
git lfs push --all hf main
```

Sau khi build xong, app se co URL dang:

```text
https://huggingface.co/spaces/<username>/phantichnhadathcm
```

File `README.md` co metadata `sdk: docker` va `app_port: 3838` de Hugging Face biet build bang Dockerfile va mo dung cong Shiny.

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

Keo data den moc muc tieu, van dung pipeline cap nhat chinh:

```bash
UPDATE_TO_TARGET=1 TARGET_ROWS=30000 DRY_RUN=1 Rscript scripts/pipeline/cap_nhat_du_lieu.R
UPDATE_TO_TARGET=1 TARGET_ROWS=30000 Rscript scripts/pipeline/cap_nhat_du_lieu.R
```

Crawler Mogi moi duoc tich hop vao pipeline, uu tien bo sung tin cho thue:

```bash
MOGI_START_PAGE=121 MOGI_UPDATE_PAGES=140 MOGI_APPEND_EXISTING=1 \
  INCLUDE_MOGI_SCRAPE=1 Rscript scripts/pipeline/cap_nhat_du_lieu.R
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
