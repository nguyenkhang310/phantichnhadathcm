---
title: Phan Tich Nha Dat TP.HCM
sdk: docker
app_port: 3838
---

# Phan Tich Nha Dat TP.HCM

Du an R/Shiny phan tich, suy luan thong ke va du doan gia bat dong san TP.HCM tu cac nguon: Cho Tot, Alonhadat, Luachonnhadat, Muaban, Mogi va Homedy.

## Cau Truc Da Don Gon

```text
phantichnhadathcm/
|-- app.R
|-- bao_cao_du_an_bds_hcm.Rmd
|-- deploy/
|   |-- Dockerfile
|   `-- render.yaml
|-- data/
|   |-- raw/       # Du lieu rieng tung nguon
|   |-- interim/   # Du lieu da gop schema chung
|   |-- main/      # data/main/du_lieu_chinh.csv
|   |-- cache/     # Cache ky thuat
|   `-- logs/      # Nhat ky cap nhat
|-- outputs/
|   |-- models/    # Model RDS, metrics, registry, phan cum
|   `-- plots/     # Bieu do PNG/CSV dung chung cho pipeline va bao cao
|-- scripts/
|   |-- 01_thu_thap_du_lieu/     # Scraper va importer
|   |-- 02_xu_ly_du_lieu/        # Merge + feature engineering
|   |-- 03_suy_luan_thong_ke/    # EDA, bootstrap, CI, kiem dinh
|   |-- 04_mo_hinh_hoa/          # Train model + K-Means
|   |-- config/                  # Duong dan trung tam
|   |-- lib/                     # Ham dung chung
|   |-- pipeline/                # Chay tu dau den cuoi
|   `-- checks/                  # Smoke test
|-- docs/                        # HTML bao cao, tai lieu va so do
|-- ung_dung/
`-- www/
```

## File Chinh

| Nhom | File |
|---|---|
| Du lieu sach | `data/main/du_lieu_chinh.csv` |
| Du lieu gop | `data/interim/du_lieu_gop_nguon.csv` |
| Output mo hinh | `outputs/models` |
| Output bieu do | `outputs/plots` |
| Bao cao R Markdown | `bao_cao_du_an_bds_hcm.Rmd` |
| App Shiny | `app.R` |
| Cau hinh deploy | `deploy/Dockerfile`, `deploy/render.yaml` |
| Cau hinh duong dan | `scripts/config/duong_dan_du_an.R` |
| Ham chuan hoa chung | `scripts/lib/chuan_hoa_du_lieu.R` |
| Ham dac trung mo hinh | `scripts/lib/dac_trung_mo_hinh.R` |

## Chay Tung Phan Rieng Le

Thu thap/import tat ca nguon:

```bash
Rscript scripts/01_thu_thap_du_lieu/thu_thap_tat_ca.R
```

Chay rieng tung scraper/importer:

```bash
Rscript scripts/01_thu_thap_du_lieu/thu_thap_chotot.R
Rscript scripts/01_thu_thap_du_lieu/thu_thap_alonhadat.R
Rscript scripts/01_thu_thap_du_lieu/thu_thap_luachonnhadat.R
Rscript scripts/01_thu_thap_du_lieu/thu_thap_muaban.R
Rscript scripts/01_thu_thap_du_lieu/thu_thap_mogi.R
Rscript scripts/01_thu_thap_du_lieu/nhap_mogi.R
Rscript scripts/01_thu_thap_du_lieu/nhap_homedy.R
```

Xu ly du lieu:

```bash
Rscript scripts/02_xu_ly_du_lieu/gop_nguon_du_lieu.R
Rscript scripts/02_xu_ly_du_lieu/tao_dac_trung.R
```

EDA va suy luan thong ke:

```bash
Rscript scripts/03_suy_luan_thong_ke/phan_tich_eda.R
Rscript scripts/03_suy_luan_thong_ke/phan_tich_suy_luan.R
```

Mo hinh hoa:

```bash
Rscript scripts/04_mo_hinh_hoa/huan_luyen_mo_hinh.R
```

Script mo hinh hoa tao them cac file audit trong `outputs/models`, gom:

- `chi_so_mo_hinh.csv`: bang RMSE, MAE, MAPE, Median APE, P90 APE va R2.
- `pham_vi_mo_hinh_audit.csv`: so dong giu lai/loai khoi tap train theo tung phan khuc.
- `dong_loai_khoi_model_ban.csv`, `dong_loai_khoi_model_thue.csv`: cac dong ngoai le duoc luu lai de ra soat.
- `du_doan_kiem_dinh_ban.csv`, `du_doan_kiem_dinh_thue.csv`: du doan tren tap test de chan doan sai so.

Chay toan bo pipeline:

```bash
Rscript scripts/pipeline/chay_pipeline.R
```

Chay app:

```bash
Rscript app.R
```

Kiem tra nhanh du an:

```bash
Rscript scripts/checks/kiem_tra_du_an.R
```

## Ghi Chu Ve Scraper

Scraper Chotot dung gateway public cua website, khong can API key rieng. Cac website HTML/API con lai co the bi chan theo thoi diem; script da co co che giu CSV cu neu lan crawl moi khong tra ve du lieu hop le, tranh ghi de thanh file rong.

## Bao Cao R Markdown

File `bao_cao_du_an_bds_hcm.Rmd` nam o thu muc goc de de thao tac cung `app.R`. Bao cao HTML duoc xuat vao `docs/bao_cao_du_an_bds_hcm.html`. Neu may co Pandoc/RStudio, render bang:

```bash
Rscript -e 'rmarkdown::render("bao_cao_du_an_bds_hcm.Rmd", output_file = "bao_cao_du_an_bds_hcm.html", output_dir = "docs")'
```

## Deploy

Cau hinh Docker/Render nam trong `deploy/`. App doc cong theo thu tu:

```text
BDS_APP_PORT > PORT > 3838
```

Chay Docker local:

```bash
docker build -f deploy/Dockerfile -t phantichnhadathcm .
docker run --rm -p 3838:3838 -e BDS_APP_PORT=3838 phantichnhadathcm
```
