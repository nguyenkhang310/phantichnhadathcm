# BAO CAO CO SO LY THUYET VA PHUONG PHAP NGHIEN CUU
## De tai: Xay dung he thong thu thap, phan tich va du doan gia bat dong san TP.HCM bang R/Shiny

> Tai lieu nay duoc viet theo dan y trong `docs/references/do_an_ket_thuc_mon_hoc_R.pdf` gom 11 phan: Tom tat, Gioi thieu, Du lieu, Truc quan hoa du lieu, Mo hinh hoa du lieu, Thuc nghiem - ket qua - thao luan, Ket luan, Phu luc, Dong gop, Tham khao va Peer assessment. Noi dung ben duoi tong hop toan bo nhung thanh phan dang co trong du an `phantichnhadathcm`, uu tien so lieu thuc te doc tu CSV/model artifact hien tai.

---

## 1. TOM TAT (ABSTRACT)

Do an xay dung mot he thong phan tich bat dong san TP.HCM bang ngon ngu R, ket hop ba lop cong viec chinh: thu thap va chuan hoa du lieu tin dang tu nhieu nguon, phan tich truc quan va suy luan thong ke, sau do huan luyen mo hinh may hoc de uoc tinh gia ban/gia thue. San pham cuoi cung duoc dong goi thanh ung dung Shiny Dashboard co nhieu tab tuong tac: Tong quan, Ban do du lieu, Phan tich gia, Suy luan thong ke, Du doan gia, Danh gia model, Phan cum khu vuc, Du lieu va Tro ly BDS local.

Tap du lieu chinh dang dung trong du an la `data/main/du_lieu_chinh.csv`, gom **30,250 tin dang** va **56 cot** sau khi gop nguon, lam sach va tao dac trung. Du lieu den tu 6 nhom nguon/kenh: Mogi, Cho Tot, Alonhadat, Luachonnhadat, Homedy va Muaban; rieng Alonhadat co ca nguon crawl truc tiep va nguon CSV local duoc import lai, Mogi co them nguon crawl bo sung uu tien tin cho thue. Trong tap du lieu chinh co **14,891 tin ban** va **15,359 tin cho thue**. Gia ban trung vi toan bo phan khuc ban la khoang **6.30 ty VND**, dien tich trung vi **76 m2**, don gia trung vi **91.21 trieu VND/m2**. Gia thue trung vi la khoang **25 trieu VND/thang**, dien tich trung vi **98 m2**, don gia thue trung vi **235.00 nghin VND/m2/thang**.

Quy trinh ETL duoc chia thanh cac buoc ro rang. Cac scraper/importer trong `scripts/scrapers/` va `scripts/importers/` tao du lieu raw theo schema noi bo. Script `scripts/processing/gop_nguon_du_lieu.R` chuan hoa 27 cot chung, loc gia/dien tich bat hop ly va khu trung lap theo `source_id` va URL tin dang. Script `scripts/processing/tao_dac_trung.R` tao cac bien phuc vu thong ke va mo hinh nhu `log_price`, `log_area`, `log_price_per_m2`, `distance_to_center`, cac bien text nhan dien mat tien/hem xe hoi/phap ly/noi that, so tang/phong suy luan, tuoi tin dang va nhom gia.

Ve mo hinh hoa, do an tach rieng hai bai toan: du doan **gia ban** va du doan **gia thue**. Moi phan khuc duoc danh gia bang 5 mo hinh: Linear Regression, Random Forest, XGBoost, RF + XGBoost Ensemble trung binh 0.5 va Tuned RF/XGBoost Ensemble co trong so toi uu. Cac mo hinh duoc validate theo ti le **80/20 phan tang theo nguon du lieu** (`stratified_random_by_source`). Theo artifact hien tai trong `models/dang_ky_mo_hinh.csv`, mo hinh tot nhat cho ca hai phan khuc la **Tuned RF/XGBoost Ensemble**. Ket qua validate hien tai:

| Phan khuc | Best model | Train validate | Test validate | RMSE | MAE | MAPE | R2 |
|---|---|---:|---:|---:|---:|---:|---:|
| Ban | Tuned RF/XGBoost Ensemble | 11,911 | 2,978 | 24.20 ty VND | 6.89 ty VND | 37.78% | 0.680 |
| Cho thue | Tuned RF/XGBoost Ensemble | 12,285 | 3,074 | 84.79 trieu VND | 26.87 trieu VND | 41.23% | 0.564 |

Ket qua cho thay mo hinh cay va ensemble tot hon hoi quy tuyen tinh, nhung sai so van con lon do du lieu bat dong san rat phan tan, gia dang tin khong phai gia giao dich thuc, nhieu loai BDS co don vi gia khac nhau va mot so dac trung quan trong nhu huong nha, phap ly chi tiet, chat luong noi that, vi tri hem/duong thuc te chua duoc cau truc hoa day du.

Ngoai du bao gia, dashboard con tich hop suy luan thong ke gom xac suat thuc nghiem, xac suat co dieu kien, ECDF, mo phong Dinh ly gioi han trung tam (CLT), bootstrap khoang tin cay cho trung vi gia/m2, Welch's t-test va Wilcoxon Rank-Sum test. Tro ly BDS trong ung dung la bo may NLP local viet bang R, khong goi API ngoai; no co kha nang nhan dien y dinh, trich xuat ngan sach/dien tich/so phong/khu vuc/loai BDS, ghi nho ngu canh hoi thoai va tra ve bang/thong tin/listing cards tu du lieu that.

---

## 2. GIOI THIEU (INTRODUCTION)

### 2.1 Boi canh va ly do chon de tai

Thi truong bat dong san TP.HCM co quy mo lon, toc do bien dong cao va thong tin phan tan tren nhieu nen tang dang tin. Mot nguoi mua, nguoi thue hoac nha dau tu muon nam duoc mat bang gia can doc nhieu website, so sanh nhieu khu vuc va tu xu ly cac van de nhu tin trung lap, gia dang ao, thieu toa do, ten quan/huyen khong dong nhat, ten loai BDS khac nhau giua cac nguon. Neu chi doc tung tin rieng le, nguoi dung de bi anh huong boi cac tin gia qua cao/thap hoac bo qua xu huong chung cua thi truong.

Trong boi canh mon hoc "Lap trinh R cho phan tich", de tai nay phu hop vi khai thac duoc nhieu nhom ky thuat da hoc:

- Doc, ghi va tien xu ly du lieu bang R.
- Thu thap du lieu tu web/API.
- Lam sach du lieu, chuan hoa nhan, loc ngoai lai.
- Truc quan hoa du lieu bang ggplot2, plotly va leaflet.
- Thong ke mo ta va suy luan thong ke.
- Mo hinh hoa hoi quy va may hoc.
- Dong goi thanh ung dung phan tich tuong tac bang Shiny.

### 2.2 Van de nghien cuu

Do an tap trung tra loi cac cau hoi sau:

1. Co the tu dong thu thap va hop nhat du lieu bat dong san TP.HCM tu nhieu nguon khac nhau thanh mot schema chung hay khong?
2. Cac khu vuc nao co nhieu tin dang nhat, gia/m2 cao nhat, co cau loai BDS ra sao?
3. Gia bat dong san co moi quan he nhu the nao voi dien tich, loai BDS, khu vuc, khoang cach den trung tam, va cac dac trung van ban trong tieu de?
4. Co the uoc tinh gia ban/gia thue bang cac mo hinh hoi quy va may hoc trong R voi sai so chap nhan duoc hay khong?
5. Nhung bat dinh trong du lieu co the duoc giai thich bang thong ke suy luan nhu bootstrap, t-test, Wilcoxon va CLT nhu the nao?
6. Co the xay dung mot dashboard de nguoi dung khong can doc code van co the xem ban do, bieu do, bang du lieu, du doan gia va hoi tro ly BDS hay khong?

### 2.3 Muc tieu do an

Do an co 6 muc tieu chinh:

1. **Xay dung pipeline du lieu**: thu thap/import du lieu tu Cho Tot, Alonhadat, Luachonnhadat, Muaban, Mogi, Homedy; luu ve `data/raw`; gop thanh `data/interim/du_lieu_gop_nguon.csv`; tao du lieu chinh `data/main/du_lieu_chinh.csv`.
2. **Chuan hoa va lam sach du lieu**: dua cac nguon ve schema chung, ep kieu cot, loc gia/dien tich bat hop ly, chuan hoa quan/huyen, tinh gia/m2, khu trung lap bang id va URL.
3. **Tao dac trung phan tich**: trich dac trung tu tieu de/dia chi, tinh khoang cach Haversine den trung tam, log-transform, tao bien thoi gian, phan khuc gia, target encoding trong qua trinh train model.
4. **Phan tich truc quan va thong ke**: tao cac bieu do offline trong `plots/` va cac bieu do tuong tac trong dashboard.
5. **Huan luyen va danh gia mo hinh du doan gia**: so sanh Linear Regression, Random Forest, XGBoost va Ensemble theo RMSE, MAE, MAPE, R2.
6. **Trien khai ung dung Shiny**: giao dien loc du lieu, ban do Leaflet, du doan gia, model diagnostics, phan cum, bang du lieu va tro ly hoi dap local.

### 2.4 Pham vi nghien cuu

Pham vi khong gian la thi truong TP.HCM theo nhan quan/huyen/khu vuc cu trong du lieu dang tin. File `scripts/lib/chuan_hoa_quan_huyen.R` quy cac cach ghi khac nhau ve nhan thi truong thong nhat, vi du "Quan 2", "Quan 9", "Thu Duc" duoc quy ve **Thanh pho Thu Duc**. Script cung co mapping mot so ten phuong moi ve khu vuc thi truong cu de dashboard de doc hon. Du lieu hien tai khong con dong "Khong ro" sau buoc chuan hoa; tap du lieu van giu **2 dong "TP Vung Tau cu"**, **1 dong "TP Ben Cat cu"** va **1 dong "Binh Duong cu"** nhu canh bao nguon dang tin co the vuot ngoai pham vi TP.HCM.

Pham vi nghiep vu gom ca **ban** va **cho thue**. Hai phan khuc duoc tach rieng khi huan luyen mo hinh vi don vi gia va dong luc thi truong khac nhau. Du lieu su dung la **gia dang tin** (asking/listing price), khong phai gia giao dich da cong chung, nen ket qua duoc hieu la mat bang rao ban/rao thue tren cac nen tang, khong phai gia chot cuoi cung.

### 2.5 Cau truc du an

```text
phantichnhadathcm/
|-- app.R
|-- README.md
|-- data/
|   |-- raw/                 # Du lieu rieng tung nguon
|   |-- interim/             # Du lieu gop nguon
|   |-- main/                # Du lieu chinh cho app/model
|   `-- logs/                # Nhat ky cap nhat
|-- scripts/
|   |-- config/              # Cau hinh path dung chung
|   |-- lib/                 # Chuan hoa quan/huyen
|   |-- scrapers/            # Thu thap tu web/API
|   |-- importers/           # Nhap CSV local ve schema chung
|   |-- processing/          # Gop nguon, tao dac trung
|   |-- analysis/            # EDA/plots
|   |-- models/              # Huan luyen model
|   |-- pipeline/            # Chay pipeline/cap nhat tu dong
|   `-- checks/              # Kiem tra nhanh app/model/tro ly
|-- ung_dung/
|   |-- ham_ho_tro_ung_dung.R
|   |-- giao_dien_ung_dung.R
|   `-- may_chu_ung_dung.R
|-- models/
|-- plots/
|-- docs/
|   |-- co_so_ly_thuyet.md
|   |-- references/
|   `-- diagrams/
`-- www/
    |-- giao_dien.css
    |-- tuong_tac.js
    `-- hcmute-logo.png
```

### 2.6 Vai tro cac file chinh

| File/thu muc | Vai tro |
|---|---|
| `app.R` | Diem vao ung dung Shiny, nap package, cau hinh port/host, source UI/server/helper. |
| `scripts/config/duong_dan_du_an.R` | Khai bao tat ca duong dan du lieu, model, plot, scraper, importer, pipeline. |
| `scripts/lib/chuan_hoa_quan_huyen.R` | Chuan hoa ten quan/huyen, bo dau tieng Viet, map ten phuong moi/nhan cu. |
| `scripts/processing/gop_nguon_du_lieu.R` | Doc raw CSV, dua ve 27 cot chung, loc ngoai lai, khu trung lap, luu interim. |
| `scripts/processing/tao_dac_trung.R` | Tao dac trung tu du lieu gop, tinh log, gia/m2, Haversine, text flags, thoi gian. |
| `scripts/analysis/phan_tich_eda.R` | Tao 8 bieu do EDA PNG va file `plots/tom_tat_eda_hcm.csv`. |
| `scripts/models/huan_luyen_mo_hinh.R` | Train/evaluate Linear, RF, XGBoost, Ensemble, K-Means, luu model/metrics. |
| `ung_dung/giao_dien_ung_dung.R` | Khai bao layout Shiny, sidebar, topbar, tabs, input/output placeholder. |
| `ung_dung/may_chu_ung_dung.R` | Server reactive, render plot/map/table, observeEvent, download report, chat. |
| `ung_dung/ham_ho_tro_ung_dung.R` | Ham xu ly data, format, du doan, thong ke, chart, assistant NLP, report PDF. |
| `www/giao_dien.css` | Style dashboard, KPI, panel, filter, assistant, responsive layout. |
| `www/tuong_tac.js` | Active nav, resize Plotly, auto-scroll chat, Enter de gui, STT/TTS tieng Viet. |

---

## 3. DU LIEU (DATA)

### 3.1 Nguon du lieu

Du lieu duoc thu thap/import tu cac nguon sau:

| Nguon | File dau ra chuan | Cach lay du lieu | So dong raw hien tai |
|---|---|---|---:|
| Cho Tot | `data/raw/chotot/chotot_schema_chuan.csv` | Goi API JSON `gateway.chotot.com`, co cache SQLite | 8,112 |
| Alonhadat crawler | `data/raw/alonhadat/alonhadat_schema_chuan.csv` | Crawl HTML bang `httr`, `rvest`, `xml2` | 62 |
| Alonhadat local | `data/raw/alonhadat/alonhadat_local_schema_chuan.csv` | Import CSV local `alonhadat_local_nguon.csv` | 1,279 |
| Luachonnhadat | `data/raw/luachonnhadat/luachonnhadat_schema_chuan.csv` | Goi API loadmore, co tuy chon fetch detail | 799 |
| Muaban | `data/raw/muaban/muaban_schema_chuan.csv` | Crawl HTML, co fallback Chrome headless neu co `chromote` | 363 |
| Mogi | `data/raw/mogi/mogi_schema_chuan.csv` | Import CSV sach/goc, CSV bo sung va CSV crawl Mogi | 19,073 |
| Homedy | `data/raw/homedy/homedy_schema_chuan.csv` | Import CSV sach/goc | 566 |
| Gop nguon | `data/interim/du_lieu_gop_nguon.csv` | Ket qua hop nhat raw | 30,250 |

Tong raw theo cac file chuan la 30,254 dong, sau loc/khu trung lap con 30,250 dong trong file interim va file main. Su khac biet 4 dong den tu viec loai trung lap hoac dong khong hop le khi gop nguon.

### 3.2 Cau hinh duong dan du lieu

Tat ca duong dan duoc tap trung trong `scripts/config/duong_dan_du_an.R`. Cach thiet ke nay giup:

- Tranh hard-code duong dan o nhieu file.
- Khi doi ten/vi tri file chi can sua mot noi.
- App, pipeline, check script va model dung chung cung mot nguon cau hinh.
- Co the mo rong nguon du lieu bang cach bo sung path moi vao `PATHS`.

Mot so path quan trong:

```r
PATHS$combined_raw_csv = "data/interim/du_lieu_gop_nguon.csv"
PATHS$featured_csv     = "data/main/du_lieu_chinh.csv"
PATHS$metrics_csv      = "models/chi_so_mo_hinh.csv"
PATHS$registry_csv     = "models/dang_ky_mo_hinh.csv"
PATHS$sale_model_rds   = "models/mo_hinh_gia_ban.rds"
PATHS$rent_model_rds   = "models/mo_hinh_gia_thue.rds"
```

### 3.3 Schema du lieu chuan sau gop nguon

Script `gop_nguon_du_lieu.R` dinh nghia 27 cot chuan trong `STANDARD_COLS`:

| Nhom cot | Cot |
|---|---|
| Dinh danh nguon | `source`, `source_group`, `source_id`, `ad_id` |
| Mo ta tin dang | `title`, `price_str`, `address`, `ward`, `district_id`, `district_name`, `category_id`, `category_name` |
| Gia va dien tich | `price`, `area`, `rooms`, `price_m`, `price_per_m2` |
| Toa do/hinh/link | `lat`, `lon`, `image`, `ad_url`, `source_url`, `has_coord` |
| Thoi gian/kiem soat | `posted_at`, `scraped_at`, `page_fetched`, `is_rent` |

Nguyen tac chuan hoa:

- Nguon nao thieu cot thi them cot do voi gia tri `NA`.
- Cot chuoi ep ve character, cot so ep ve numeric, cot co/khong ep ve integer.
- Neu thieu `source_id`, he thong tao tu `source` va `ad_id`.
- Neu thieu `source_url`, dung lai `ad_url`.
- URL duoc chuan hoa bang cach trim, bo dau `/` cuoi va dua ve lowercase de phuc vu khu trung lap.

### 3.4 Quy tac lam sach va loc nghiep vu

Do du lieu tin dang thu thap tu web co nhieu ngoai lai, script gop nguon va tao dac trung ap dung cac quy tac:

| Dieu kien | Gia tri giu lai |
|---|---|
| Gia ban | Tu 300,000,000 den 500,000,000,000 VND |
| Gia thue | Tu 300,000 den 2,000,000,000 VND/thang |
| Dien tich | Tu 5 den 5,000 m2 |
| Gia tri `price` | Khong duoc `NA`, phai lon hon 0 |
| Trung lap cap 1 | `distinct(source_id, .keep_all = TRUE)` |
| Trung lap cap 2 | Chuan hoa `ad_url` thanh `ad_url_key`, sau do `distinct(ad_url_key, .keep_all = TRUE)` |

O buoc tao dac trung, toa do chi duoc giu neu nam trong bounding box mo rong cua TP.HCM:

```text
10.30 <= latitude  <= 11.20
106.00 <= longitude <= 107.30
```

Neu toa do khong hop le, cot `lat`/`lon` bi gan `NA`. Khi hien thi ban do, app uoc luong toa do bang tam quan/huyen va them jitter nho de cac marker khong chong len nhau.

### 3.5 Chuan hoa quan/huyen

File `scripts/lib/chuan_hoa_quan_huyen.R` giai quyet bai toan ten khu vuc khong dong nhat. Cac ham chinh:

- `district_strip_vietnamese()`: bo dau tieng Viet, dua ve lowercase.
- `district_missing_label()`: phat hien nhan rong/khong ro/NA/null.
- `district_clean_label()`: thay nhan thieu bang "Khong ro".
- `canonical_hcmc_district_one()`: tim quan/huyen tu current label, address, title, URL.
- `canonical_hcmc_district()`: vector hoa ham chuan hoa cho toan bo cot.

Mot so quy tac dang chu y:

- `Quan 2`, `Quan 9`, `Thu Duc`, `TP Thu Duc` duoc quy ve **Thanh pho Thu Duc**.
- Cac cach ghi `q7`, `quan 7`, `Quận 7` duoc quy ve **Quận 7**.
- Ten quan chu nhu `Binh Thanh`, `Go Vap`, `Tan Phu` duoc quy ve nhan co dau day du.
- Mot so phuong moi nam 2025 duoc gan ve khu vuc thi truong cu, giup bao cao/doc bieu do phu hop voi thoi quen thi truong.

### 3.6 Tap du lieu chinh sau feature engineering

File `data/main/du_lieu_chinh.csv` co **30,250 dong** va **56 cot**. Ngoai 27 cot schema goc, file nay co them cac cot dac trung:

| Nhom dac trung | Cot |
|---|---|
| Kich thuoc trich tu text | `frontage_width_m`, `frontage_length_m`, `frontage_ratio` |
| So tang/phong suy luan | `inferred_floors`, `inferred_rooms` |
| Bien nhi phan tu tieu de | `title_has_frontage`, `title_has_alley`, `title_has_car_access`, `title_has_corner`, `title_has_elevator`, `title_has_furnished`, `title_has_legal`, `title_has_income_info` |
| Do dai text | `title_token_count` |
| Co du lieu goc | `has_area`, `has_rooms`, `has_coord` |
| Giao dich | `transaction_type` |
| Thong ke phuong | `ward_median_price`, `ward_listing_count`, `ward_price_encoded` |
| Bien log | `log_price`, `log_area`, `log_price_per_m2` |
| Khong gian/thoi gian | `distance_to_center`, `listing_age_days`, `posted_hour`, `posted_wday`, `is_weekend_post` |
| Phan khuc gia | `price_segment` |

### 3.7 Phan bo du lieu theo nguon

| Nguon | So tin | Ty trong xap xi |
|---|---:|---:|
| Mogi | 19,073 | 63.1% |
| Cho Tot | 8,108 | 26.8% |
| Alonhadat | 1,341 | 4.4% |
| Luachonnhadat | 799 | 2.6% |
| Homedy | 566 | 1.9% |
| Muaban | 363 | 1.2% |

Theo giao dich:

| Nguon | Ban | Cho thue | Tong |
|---|---:|---:|---:|
| Alonhadat | 1,313 | 28 | 1,341 |
| Cho Tot | 8,018 | 90 | 8,108 |
| Homedy | 390 | 176 | 566 |
| Luachonnhadat | 400 | 399 | 799 |
| Mogi | 4,591 | 14,482 | 19,073 |
| Muaban | 179 | 184 | 363 |
| **Tong** | **14,891** | **15,359** | **30,250** |

Bang doi chieu chi tiet theo tung website trong file data chinh:

| Website/nguon | Raw chuan | Vao `du_lieu_chinh.csv` | Ban | Cho thue | Toa do goc | Toa do uoc luong | Ty le toa do goc |
|---|---:|---:|---:|---:|---:|---:|---:|
| Cho Tot | 8,112 | 8,108 | 8,018 | 90 | 8,044 | 64 | 99.2% |
| Alonhadat | 1,341 | 1,341 | 1,313 | 28 | 0 | 1,341 | 0.0% |
| Luachonnhadat | 799 | 799 | 400 | 399 | 0 | 799 | 0.0% |
| Muaban | 363 | 363 | 179 | 184 | 0 | 363 | 0.0% |
| Mogi | 19,073 | 19,073 | 4,591 | 14,482 | 6,200 | 12,873 | 32.5% |
| Homedy | 566 | 566 | 390 | 176 | 557 | 9 | 98.4% |
| **Tong** | **30,254** | **30,250** | **14,891** | **15,359** | **14,801** | **15,449** | **48.9%** |

Ghi chu: "Raw chuan" la so dong trong cac file `*_schema_chuan.csv` truoc khi hop nhat/khu trung lap. "Vao du_lieu_chinh.csv" la so dong sau khi gop nguon, loc nghiep vu va tao dac trung. Cot "toa do uoc luong" khong co nghia la sai, ma la cac dong khong co lat/lon goc hop le nen app gan ve tam khu vuc cu va them jitter nho de hien thi tren ban do.

Nhan xet:

- Mogi la nguon dong gop lon nhat; phan crawl bo sung giup tang manh tin cho thue va lam can bang hon hai phan khuc ban/thue.
- Cho Tot va Alonhadat nghieng manh ve tin ban.
- Luachonnhadat va Muaban gan can bang giua ban va thue.
- Ty trong nguon khong dong deu co the tao bias cho thong ke va mo hinh, nen train/test duoc chia phan tang theo `source`.

### 3.8 Phan bo theo khu vuc

Top khu vuc co nhieu tin nhat:

| Khu vuc | So tin |
|---|---:|
| Thanh pho Thu Duc | 2,780 |
| Quan 7 | 2,150 |
| Quan Binh Tan | 1,798 |
| Quan Binh Thanh | 1,424 |
| Quan Tan Binh | 1,383 |
| Quan 12 | 1,109 |
| Quan Tan Phu | 1,071 |
| Quan 1 | 1,070 |
| Quan Go Vap | 1,057 |
| Quan 10 | 904 |
| Quan 3 | 877 |
| Quan Phu Nhuan | 783 |

Nhan xet:

- Thanh pho Thu Duc va Quan 7 la hai khu vuc co do phu lon, phu hop cho phan tich thong ke va so sanh.
- Mot so huyen ven nhu Can Gio co rat it tin, nen ket luan tai cac khu vuc nay can than trong.
- Khong con dong khu vuc `Khong ro`; cac dong ngoai TP.HCM duoc giu nhan canh bao rieng nhu `TP Vung Tau cu`, `TP Ben Cat cu`, `Binh Duong cu`.

### 3.9 Phan bo theo loai bat dong san

Top nhan loai BDS theo so tin:

| Loai BDS | So tin |
|---|---:|
| Nha pho | 4,456 |
| Nha o | 4,158 |
| Can ho | 2,799 |
| Can ho/Chung cu | 2,657 |
| Dat | 2,577 |
| Bat dong san khac | 2,392 |
| Phong/Cho thue | 650 |
| Dat nen | 624 |
| Biet thu | 131 |
| Van phong, Mat bang kinh doanh | 90 |
| Can ho chung cu | 69 |

Nhan xet quan trong: nhan loai BDS hien tai van phan anh cach goi cua tung nguon, vi du `Can ho`, `Can ho/Chung cu`, `Can ho chung cu` co y nghia gan nhau nhung chua duoc gop hoan toan. Dieu nay giup giu thong tin goc, nhung khi viet bao cao nen ghi ro rang day la "nhan loai sau chuan hoa co ban", chua phai taxonomy duy nhat tuyet doi.

### 3.10 Chat luong du lieu hien tai

| Chi so | Gia tri |
|---|---:|
| Tong dong main | 30,250 |
| So cot main | 56 |
| Dong thieu/khong ro khu vuc | 0 |
| Dong thieu loai BDS | 0 |
| Dong co toa do hop le | 14,801 |
| Dong thieu toa do hop le | 15,449 |
| Ty le toa do goc hop le | 48.9% |
| Ngay dang nho nhat | 2023-06-01 |
| Ngay dang lon nhat | 2026-06-18 |
| Ngay dang trong tuong lai | 0 dong |

Dashboard khong tu dong xoa cac canh bao nhu toa do uoc luong hay outlier gia/dien tich, ma dua vao tab **Du lieu** de nguoi dung doc ket qua can than. Rieng ngay dang tuong lai da duoc chuan hoa ve ngay crawl hop le neu nguon tra ve sai nam. Phan Mogi crawl bo sung hien uu tien listing-level nen nhieu dong khong co toa do goc; khi len ban do, app uoc luong theo tam khu vuc va jitter nho.

### 3.11 Ly thuyet ve cac bien dac trung

#### 3.11.1 Gia tren met vuong

Gia tong bi anh huong rat lon boi dien tich. Vi vay bien `price_per_m2` duoc tinh:

```text
price_per_m2 = price / area
```

Chi so nay giup so sanh mat bang gia giua cac khu vuc/loai BDS co dien tich khac nhau. Doi voi cho thue, `price_per_m2` duoc hieu la VND/m2/thang.

#### 3.11.2 Bien doi log

Gia bat dong san thuong lech phai rat manh: phan lon tin nam o muc pho thong/trung cap, nhung mot so tin biet thu/nha pho trung tam co gia cuc cao. Neu dung gia goc, mo hinh de bi chi phoi boi outlier. Do do he thong tao:

```text
log_price        = log(1 + price)
log_area         = log(1 + area)
log_price_per_m2 = log(1 + price_per_m2)
```

Khi can dua du doan ve VND:

```text
predicted_price = exp(predicted_log_price) - 1
```

#### 3.11.3 Trich dac trung tu tieu de

Script `tao_dac_trung.R` ghep `title`, `address`, `category_name` thanh `text_raw`, sau do bo dau tieng Viet thanh `text_key`. Cac bieu thuc regex duoc dung de trich:

- Kich thuoc ngang x dai: `4x15`, `5 x 20`, `4.5x18`.
- So tang/lau/tam: `3 tang`, `2 lau`, `h + 4`.
- So phong ngu: `2PN`, `3 phong ngu`.
- Tu khoa mat tien/mat pho/mat duong.
- Tu khoa hem/ngo/HXH.
- Tu khoa xe hoi/o to/oto.
- Tu khoa goc/2 mat/hai mat.
- Tu khoa thang may.
- Tu khoa noi that/full noi that/full nt/furnished.
- Tu khoa so hong/phap ly/giay to/hop le.
- Tu khoa hop dong/dong tien/cho thue.

Cac bien nhi phan nhan gia tri 0 hoac 1, vi du:

```text
title_has_frontage = 1 neu tieu de co mat tien/mat pho/mat duong
title_has_car_access = 1 neu tieu de co HXH/xe hoi/o to
title_has_legal = 1 neu tieu de co so hong/phap ly/giay to
```

#### 3.11.4 Khoang cach Haversine den trung tam

Bien `distance_to_center` tinh khoang cach tu tin dang den diem tham chieu trung tam TP.HCM, trong code la toa do xap xi khu vuc trung tam Quan 1:

```text
lat_center = 10.7758
lon_center = 106.7009
R = 6371 km
```

Cong thuc Haversine:

```text
d = 2R * atan2(sqrt(a), sqrt(1-a))
a = sin^2(delta_lat/2) + cos(lat1) * cos(lat2) * sin^2(delta_lon/2)
```

Trong bat dong san, khoang cach den trung tam thuong co tuong quan am voi gia/m2: cang xa trung tam thi gia trung vi co xu huong giam, tuy khong phai luc nao cung dung vi con phu thuoc ha tang, quy hoach, loai BDS va khu do thi moi.

#### 3.11.5 Target encoding trong huan luyen model

Trong file main, `ward_price_encoded` ban dau duoc tao tu trung vi gia theo phuong. Tuy nhien, khi huan luyen model, script `huan_luyen_mo_hinh.R` refit target encoding **chi tren tap train** de giam nguy co ro ri du lieu. Ham `fit_target_encoding()` dung cong thuc lam muot:

```text
encoded_value = log1p((n_g * median_g + smoothing * global_median) / (n_g + smoothing))
```

Trong do:

- `n_g`: so mau trong nhom g.
- `median_g`: trung vi gia cua nhom g trong train.
- `global_median`: trung vi gia toan tap train.
- `smoothing = 10`: trong so keo nhom it mau ve trung vi toan cuc.

Model dung 6 bang encoding:

- `ward_price_encoded`
- `district_price_encoded`
- `category_price_encoded`
- `source_price_encoded`
- `district_category_price_encoded`
- `source_category_price_encoded`

Day la diem quan trong khi bao ve do an: target encoding khong tinh tren toan bo train + test luc validate, ma fit bang train roi apply sang test.

---

## 4. TRUC QUAN HOA DU LIEU (DATA VISUALIZATION)

### 4.1 Muc tieu truc quan hoa

Truc quan hoa giup chuyen tap du lieu 30,250 dong thanh cac pattern de doc:

- Xem phan bo gia co lech khong.
- So sanh so tin theo khu vuc/loai BDS.
- Nhin moi quan he giua dien tich va gia.
- Phat hien khu vuc co gia/m2 cao/thap.
- Xem phan bo khong gian bang toa do.
- Theo doi xu huong so tin/gia theo thoi gian.
- Ho tro suy luan thong ke va danh gia mo hinh.

Do an co hai lop truc quan hoa: **offline plots** trong `plots/` va **interactive dashboard** trong Shiny.

### 4.2 Cac bieu do offline trong `plots/`

Script `scripts/analysis/phan_tich_eda.R` tao 8 bieu do PNG va mot file tong hop CSV.

| File | Noi dung | Y nghia |
|---|---|---|
| `01_phan_phoi_log_gia.png` | Histogram `log1p(price)` | Kiem tra phan phoi gia sau log-transform. |
| `02_gia_theo_quan_boxplot.png` | Boxplot gia theo quan/huyen | So sanh bien do gia va outlier theo khu vuc. |
| `03_dien_tich_va_gia.png` | Scatter dien tich - gia, mau theo loai BDS | Nhin quan he giua quy mo va gia tri. |
| `04_top_quan_nhieu_tin.png` | Bar chart top 10 quan nhieu tin | Xac dinh khu vuc co thanh khoan/nguon cung du lieu cao. |
| `05_top_quan_gia_m2_cao.png` | Bar chart top 10 quan gia/m2 cao | Nhan dien khu vuc cao cap/trung tam. |
| `06_xu_huong_tin_theo_ngay.png` | Line chart so tin theo ngay | Theo doi thoi gian dang tin. |
| `07_phan_bo_dia_ly_gia.png` | Scatter lat/lon mau theo log gia | Nhin phan bo khong gian va gradient gia. |
| `08_gia_m2_theo_loai_bds.png` | Boxplot gia/m2 theo loai BDS | So sanh mat bang don gia giua cac loai. |
| `tom_tat_eda_hcm.csv` | Bang district x category | Luu listing count, median price, median area, median price/m2. |

### 4.3 Dashboard Tong quan

Tab **Tong quan** trong Shiny co cac thanh phan:

- KPI tong so tin dang sau lam sach.
- Gia trung vi toan TP.HCM.
- So khu vuc co du lieu.
- Mo hinh tot nhat theo MAPE/RMSE.
- Bieu do top khu vuc co nhieu tin theo giao dich.
- Bieu do co cau loai BDS theo giao dich.
- Bang metric model doc tu `models/chi_so_mo_hinh.csv`.

Y nghia: day la man hinh mo dau giup nguoi xem nam nhanh quy mo du lieu, do phu khu vuc va chat luong mo hinh.

### 4.4 Dashboard Ban do du lieu

Tab **Ban do du lieu** dung `leaflet` de ve marker tin dang tren ban do.

Bo loc gom:

- Nguon.
- Giao dich.
- Khu vuc.
- Loai BDS.
- Khoang gia.
- Khoang dien tich.

Marker co mau theo muc gia:

- Gia thap: xanh.
- Gia trung binh: vang/cam.
- Gia cao: do.

Popup hien:

- Tieu de tin.
- Khu vuc/phuong.
- Gia.
- Dien tich.
- Gia/m2.
- Loai BDS.
- Tinh trang toa do: toa do goc tu nguon hay uoc luong theo khu vuc.
- Link goc neu co.

Y nghia: ban do giup nhin ngay su tap trung tin dang, cac cum gia cao va cac vung co du lieu thieu toa do.

### 4.5 Dashboard Phan tich gia

Tab **Phan tich gia** la trung tam EDA tuong tac. Cac bieu do:

| Bieu do | Bien/chuc nang | Y nghia |
|---|---|---|
| Dien tich vs Gia | Scatter area - price | Kiem tra moi quan he phi tuyen, outlier va khac biet theo loai BDS. |
| Top khu vuc theo gia/m2 | Median price/m2 theo district | Tim nhom khu vuc co mat bang don gia cao. |
| Khoang gia theo loai BDS | Median va IQR theo category | So sanh phan khuc san pham. |
| Phan phoi gia | Histogram log-price | Xem do lech va tac dung cua log-transform. |
| Heatmap khu vuc x loai BDS | Median price/m2 | Nhin giao diem khu vuc - loai BDS nao dat/re. |
| Sunburst nguon du lieu | Source -> giao dich -> category | Xem co cau du lieu theo nguon. |
| Xu huong theo thoi gian | Bar so tin + line median price/m2 theo thang | Theo doi bien dong tin va gia/m2. |
| Tuong quan bien so | Correlation matrix | Nhin quan he giua log price, log area, rooms, distance, listing age, token count. |
| ECDF gia/m2 | Phan phoi tich luy theo top district | So sanh percentile giua cac khu vuc. |

### 4.6 Dashboard Suy luan thong ke

Tab **Suy luan thong ke** gan truc tiep voi phan xac suat/thong ke:

- KPI co mau thong ke.
- Trung vi gia/m2.
- Standard error tren log(gia/m2).
- Xac suat gia cao theo nguong Q3.
- Heatmap xac suat co dieu kien `P(category | district)`.
- ECDF so sanh hai khu vuc.
- CLT simulation bang bootstrap mean.
- Bootstrap CI cho trung vi gia/m2.
- Bang kiem dinh gia thuyet gom Welch's t-test va Wilcoxon.
- Bang xac suat thuc nghiem.

### 4.7 Dashboard Du doan gia va Danh gia model

Tab **Du doan gia** co form:

- Khu vuc cu.
- Loai BDS.
- Giao dich ban/cho thue.
- Phuong/xa.
- Dien tich.
- So phong.

Khi bam "Tinh lai du doan", app tao mot row dau vao bang `build_prediction_row()`, nap model ban/thue tu RDS, apply encoding va du doan gia. Ket qua di kem:

- Gia du doan.
- Ghi chu model dang dung.
- Vung gia tham khao Q1/Median/Q3 tu cac listing tuong dong.
- Feature importance tu Random Forest.

Tab **Danh gia model** co:

- Model card: phan khuc, best model, MAPE sanity check, SD residual log.
- Actual vs Predicted.
- Residual distribution.
- Sai so theo khu vuc.
- So sanh metric model.

### 4.8 Dashboard Phan cum khu vuc

Tab **Phan cum khu vuc** doc `models/cum_gia_quan_huyen.csv` va ve bubble chart:

- Truc x: dien tich trung vi.
- Truc y: gia/m2 trung vi.
- Mau: cluster K-Means.
- Kich thuoc bubble: so tin.
- Tooltip: giao dich, khu vuc, loai BDS, cluster, dien tich, gia/m2, so tin.

Khac voi ban thao cu, code hien tai phan cum tren cap **transaction_type + district_name + category_name**, khong chi gom 24 quan/huyen. Moi phan khuc ban/thue duoc chay K-Means rieng, so cum toi da la 4.

### 4.9 Dashboard Du lieu

Tab **Du lieu** hien:

- Bo loc nguon, giao dich, khu vuc, loai BDS, khoang gia.
- KPI dong sau loc, ty le toa do goc, so dong ngay tuong lai, trung nghi ngo.
- Bang data quality.
- Bieu do do phu nguon du lieu va ty le toa do goc theo nguon.
- Bang DT co tim kiem va link tin goc.

### 4.10 Dashboard Tro ly BDS

Tab **Tro ly BDS** co giao dien chat local:

- Loi chao va goi y cau hoi mau.
- Khung nhap cau hoi.
- Nut gui.
- Nut mic dung SpeechRecognition tieng Viet neu trinh duyet ho tro.
- Nut doc cau tra loi bang Web Speech API.
- Memory hoi thoai.
- HTML response gom KPI, bang, insight block va listing cards.

Tro ly khong goi API ngoai. Tat ca cau tra loi duoc tao tu du lieu/model local trong project.

---

## 5. MO HINH HOA DU LIEU (DATA MODELING)

### 5.1 Bai toan du doan

Do an co hai bai toan hoi quy:

1. Du doan **gia ban** cho cac tin co `is_rent = FALSE`.
2. Du doan **gia thue** cho cac tin co `is_rent = TRUE`.

Bien muc tieu cua ca hai bai toan la `log_price = log1p(price)`. Dung log-price giup mo hinh on dinh hon voi cac muc gia rat lon.

### 5.2 Tap dac trung cho mo hinh

Cong thuc mo hinh cuoi cung trong RDS hien tai:

```r
log_price ~ area + log_area + rooms + inferred_rooms +
  frontage_width_m + frontage_length_m + frontage_ratio +
  inferred_floors + title_has_frontage + title_has_alley +
  title_has_car_access + title_has_corner + title_has_elevator +
  title_has_furnished + title_has_legal + title_has_income_info +
  title_token_count + posted_hour + distance_to_center +
  ward_price_encoded + district_price_encoded +
  category_price_encoded + source_price_encoded +
  district_category_price_encoded + source_category_price_encoded +
  listing_age_days + source + district_name + category_name + posted_wday
```

Nhom bien:

- **Quy mo**: `area`, `log_area`, `rooms`, `inferred_rooms`, `inferred_floors`.
- **Hinh hoc nha dat**: `frontage_width_m`, `frontage_length_m`, `frontage_ratio`.
- **Tu khoa gia tri trong title**: mat tien, hem, xe hoi, goc, thang may, noi that, phap ly, dong tien.
- **Van ban**: `title_token_count`.
- **Khong gian**: `distance_to_center`.
- **Thoi gian**: `posted_hour`, `posted_wday`, `listing_age_days`.
- **Gia khu vuc/nguon da encode**: ward, district, category, source, district-category, source-category.
- **Bien phan loai**: `source`, `district_name`, `category_name`, `posted_wday`.

### 5.3 Chia train/test

Ham `make_split()` chia du lieu theo ti le 80/20, phan tang theo `source`:

- Moi nguon du lieu duoc shuffle bang `runif`.
- Neu nhom nguon co tu 5 dong tro len, lay xap xi 80% train va giu toi thieu 1 dong test.
- Neu so dong qua it, fallback chia random theo ti le.

Trong artifact hien tai:

| Phan khuc | Train validate | Test validate | Split type |
|---|---:|---:|---|
| Ban | 11,911 | 2,978 | `stratified_random_by_source` |
| Cho thue | 12,285 | 3,074 | `stratified_random_by_source` |

Sau khi validate, model cuoi cung duoc refit tren **100% du lieu sach** cua tung phan khuc:

- Ban: 14,891 dong.
- Cho thue: 15,359 dong.

### 5.4 Hoi quy tuyen tinh (Linear Regression)

Linear Regression duoc dung lam baseline. Mo hinh co dang:

```text
log_price = beta0 + beta1*x1 + beta2*x2 + ... + betap*xp + epsilon
```

Uoc luong OLS:

```text
beta_hat = (X'X)^(-1) X'Y
```

Uu diem:

- De giai thich.
- Train nhanh.
- Lam baseline tot de so sanh.

Han che:

- Gia bat dong san co quan he phi tuyen voi dien tich/khu vuc/loai BDS.
- Rat nhay voi outlier.
- Kho xu ly tuong tac phuc tap giua khu vuc va loai BDS.

Ket qua thuc te cho thay Linear Regression kem hon cac mo hinh cay: MAPE ban 52.17%, MAPE cho thue 47.65%.

### 5.5 Random Forest Regression

Random Forest la tap hop nhieu cay quyet dinh hoi quy. Trong code:

```r
randomForest(formula, data = train, ntree = 500, importance = TRUE)
```

Cong thuc du doan:

```text
f_RF(x) = (1/B) * sum(T_b(x)), voi B = 500
```

Uu diem:

- Hoc duoc quan he phi tuyen.
- Tu dong xu ly tuong tac giua bien.
- Giam phuong sai bang bagging.
- Co feature importance bang `IncNodePurity` va `%IncMSE`.

Han che:

- Mo hinh lon, giai thich kem hon OLS.
- Co the du doan kem o cac vung it mau.
- Khong ngoai suy tot ngoai mien du lieu train.

### 5.6 XGBoost Regression

XGBoost dung gradient boosting, them cay moi de sua sai so cua cac cay truoc:

```text
y_hat_i^(t) = y_hat_i^(t-1) + eta * f_t(x_i)
```

Trong code:

```r
xgboost(
  objective = "reg:squarederror",
  nthread = 1,
  verbosity = 0
)
```

Du lieu dau vao XGBoost duoc chuyen thanh sparse matrix bang:

```r
sparse.model.matrix(rhs_formula, data = data)
```

Luoi tham so gom 8 ung vien:

| Ung vien | nrounds | learning_rate | max_depth | min_child_weight | subsample | colsample_bytree |
|---:|---:|---:|---:|---:|---:|---:|
| 1 | 220 | 0.08 | 5 | 1 | 0.85 | 0.85 |
| 2 | 300 | 0.05 | 6 | 1 | 0.80 | 0.80 |
| 3 | 420 | 0.03 | 5 | 1 | 0.85 | 0.85 |
| 4 | 260 | 0.06 | 4 | 3 | 0.90 | 0.90 |
| 5 | 240 | 0.05 | 7 | 2 | 0.80 | 0.80 |
| 6 | 180 | 0.10 | 4 | 1 | 0.90 | 0.75 |
| 7 | 500 | 0.025 | 6 | 2 | 0.85 | 0.85 |
| 8 | 360 | 0.04 | 7 | 1 | 0.78 | 0.82 |

Ca model ban va thue hien tai chon bo tham so:

```text
nrounds = 240
learning_rate = 0.05
max_depth = 7
min_child_weight = 2
subsample = 0.80
colsample_bytree = 0.80
```

### 5.7 Ensemble RF + XGBoost

Do an thu hai cach ensemble:

1. **RF + XGBoost Ensemble**: trung binh 0.5:

```text
y_hat = 0.5 * y_hat_RF + 0.5 * y_hat_XGB
```

2. **Tuned RF/XGBoost Ensemble**: quet `weight_rf` tu 0 den 1 voi buoc 0.05:

```text
y_hat = w * y_hat_RF + (1 - w) * y_hat_XGB
```

Trong so hien tai:

| Phan khuc | Best model | weight_rf | weight_xgb |
|---|---|---:|---:|
| Ban | Tuned RF/XGBoost Ensemble | 0.45 | 0.55 |
| Cho thue | Tuned RF/XGBoost Ensemble | 0.55 | 0.45 |

### 5.8 Gioi han du doan bang quantile log-price

De tranh model tra ket qua qua xa mien train, prediction log duoc clamp trong khoang quantile 1% va 99% cua `log_price` tren train:

| Phan khuc | Bound log_price |
|---|---|
| Ban | 20.229 den 26.393 |
| Cho thue | 14.116 den 19.799 |

Khi dua ve VND, app dung `expm1(pred_log)`.

### 5.9 Feature importance

Random Forest importance duoc luu vao:

- `models/do_quan_trong_bien_ban.csv`
- `models/do_quan_trong_bien_thue.csv`

Top bien quan trong nhat theo `IncNodePurity` cho phan khuc ban:

| Hang | Bien | IncNodePurity |
|---:|---|---:|
| 1 | `district_category_price_encoded` | 3812.35 |
| 2 | `log_area` | 3763.30 |
| 3 | `area` | 3452.16 |
| 4 | `district_name` | 1687.07 |
| 5 | `ward_price_encoded` | 1408.23 |
| 6 | `inferred_rooms` | 915.55 |
| 7 | `distance_to_center` | 850.55 |
| 8 | `rooms` | 829.51 |
| 9 | `district_price_encoded` | 767.01 |
| 10 | `source_category_price_encoded` | 749.04 |

Top bien quan trong nhat cho phan khuc cho thue:

| Hang | Bien | IncNodePurity |
|---:|---|---:|
| 1 | `area` | 2526.89 |
| 2 | `log_area` | 2482.24 |
| 3 | `district_category_price_encoded` | 924.10 |
| 4 | `category_name` | 409.07 |
| 5 | `district_name` | 403.94 |
| 6 | `source_category_price_encoded` | 383.15 |
| 7 | `category_price_encoded` | 326.14 |
| 8 | `ward_price_encoded` | 290.79 |
| 9 | `distance_to_center` | 235.02 |
| 10 | `inferred_rooms` | 192.07 |

Nhan xet: dien tich, mat bang gia theo khu vuc/loai BDS va khoang cach den trung tam la cac nhom bien rat quan trong. Dieu nay phu hop voi logic thi truong bat dong san.

### 5.10 K-Means clustering

Ngoai du doan gia, script model tao phan cum K-Means de nhom cac "o thi truong" theo:

- `median_price_per_m2`
- `median_area`
- `listing_count`

Du lieu phan cum duoc group theo:

```text
transaction_type + district_name + category_name
```

Dieu kien loc:

- Phan khuc cho thue: moi nhom can toi thieu 2 tin.
- Phan khuc ban: moi nhom can toi thieu 5 tin.

So cum:

```r
k = min(4, nrow(tx_df))
kmeans(cluster_input, centers = k, nstart = 25)
```

Ket qua hien tai:

| Giao dich | Cluster 1 | Cluster 2 | Cluster 3 | Cluster 4 | Tong nhom |
|---|---:|---:|---:|---:|---:|
| Ban | 17 | 6 | 107 | 24 | 154 |
| Cho thue | 7 | 138 | 5 | 1 | 151 |
| **Tong** | **24** | **144** | **112** | **25** | **305** |

### 5.11 Tro ly BDS local nhu mot module NLP ung dung

Tro ly khong phai mo hinh ngon ngu lon, ma la engine NLP local bang R. Cac buoc xu ly:

1. Chuan hoa cau hoi bang `assistant_text_key()`: bo dau, lowercase, loai ky tu dac biet.
2. Trich thuc the:
   - Ngan sach: `4 ty`, `duoi 15 trieu`, `tu 3 den 5 ty`.
   - Dien tich: `60m2`, `50-70m2`.
   - So phong: `2PN`, `3 phong ngu`.
   - Giao dich: mua/ban/cho thue.
   - Khu vuc: so khop danh sach district trong data.
   - Loai BDS: so khop category va alias.
3. Nhan dien y dinh:
   - `help`
   - `explain`
   - `predict`
   - `compare`
   - `undervalued`
   - `scout`
   - `recommend`
   - `stats`
4. Gop voi context cau hoi truoc neu la follow-up.
5. Goi "tool" local: loc data, tinh thong ke, xep hang listing, tim deal, so sanh khu vuc hoac goi model du doan.
6. Sinh HTML de hien thi trong Shiny.

Vi du:

- "4 ty mua can ho tam 60m2 o khu nao on?" -> intent `scout`.
- "So sanh Thu Duc voi Quan 7 cho can ho ban" -> intent `compare`.
- "Tim tin gia tot hon mat bang o Binh Tan duoi 4 ty" -> intent `undervalued`.
- "Du doan can ho 70m2 2PN o Thu Duc" -> intent `predict`.
- "Con Quan 7 thi sao?" -> follow-up giu ngan sach/loai giao dich truoc va them Quan 7.

---

## 6. THUC NGHIEM, KET QUA VA THAO LUAN

### 6.1 Thiet lap thuc nghiem

Du lieu dau vao: `data/main/du_lieu_chinh.csv`, 30,250 dong, 56 cot.

Tach phan khuc:

| Phan khuc | So dong |
|---|---:|
| Ban | 14,891 |
| Cho thue | 15,359 |

Chia validate:

- 80% train, 20% test.
- Phan tang theo `source`.
- Seed co dinh `set.seed(42)`.

Model so sanh:

1. Linear Regression.
2. Random Forest.
3. XGBoost.
4. RF + XGBoost Ensemble.
5. Tuned RF/XGBoost Ensemble.

### 6.2 Chi so danh gia

#### RMSE

```text
RMSE = sqrt(mean((actual - predicted)^2))
```

RMSE phat nang sai so lon, phu hop de xem model bi anh huong boi cac tin sieu cao cap/ngoai lai ra sao.

#### MAE

```text
MAE = mean(abs(actual - predicted))
```

MAE de giai thich hon RMSE vi la sai so tuyet doi trung binh theo VND.

#### MAPE

```text
MAPE = mean(abs((actual - predicted) / actual))
```

MAPE do sai so theo ty le phan tram, de trinh bay voi nguoi dung. Script chon best model bang MAPE truoc, neu bang nhau moi xet RMSE.

#### R2

```text
R2 = 1 - SSE/SST
```

R2 cho biet ty le bien thien cua gia duoc model giai thich.

### 6.3 Ket qua phan khuc ban

| Model | Train | Test | RMSE | MAE | MAPE | R2 |
|---|---:|---:|---:|---:|---:|---:|
| Linear Regression | 11,911 | 2,978 | 26.50 ty | 8.20 ty | 50.00% | 0.616 |
| Random Forest | 11,911 | 2,978 | 24.72 ty | 7.04 ty | 38.22% | 0.666 |
| XGBoost | 11,911 | 2,978 | 23.63 ty | 6.81 ty | 38.84% | 0.695 |
| RF + XGBoost Ensemble | 11,911 | 2,978 | 24.05 ty | 6.85 ty | 37.87% | 0.684 |
| Tuned RF/XGBoost Ensemble | 11,911 | 2,978 | 24.20 ty | 6.89 ty | 37.78% | 0.680 |

Nhan xet:

- Linear Regression kem nhat ve MAPE do khong nam bat tot quan he phi tuyen.
- XGBoost co RMSE va R2 tot nhat trong cac model don le.
- Tuned Ensemble duoc registry chon vi MAPE thap nhat va RMSE canh tranh.
- MAPE 37.78% cho thay bai toan du doan gia ban con kho, dac biet do cac tin nha pho/trung tam/biet thu co gia rat cao.

### 6.4 Ket qua phan khuc cho thue

| Model | Train | Test | RMSE | MAE | MAPE | R2 |
|---|---:|---:|---:|---:|---:|---:|
| Linear Regression | 12,285 | 3,074 | 90.42 trieu | 30.88 trieu | 57.77% | 0.505 |
| Random Forest | 12,285 | 3,074 | 85.21 trieu | 27.10 trieu | 41.59% | 0.561 |
| XGBoost | 12,285 | 3,074 | 84.82 trieu | 27.36 trieu | 43.06% | 0.564 |
| RF + XGBoost Ensemble | 12,285 | 3,074 | 84.65 trieu | 26.92 trieu | 41.42% | 0.566 |
| Tuned RF/XGBoost Ensemble | 12,285 | 3,074 | 84.79 trieu | 26.87 trieu | 41.23% | 0.564 |

Nhan xet:

- Cho thue co them nhieu dong Mogi crawl, nen validation hien tai danh gia tren tap lon va da dang hon truoc.
- RF + XGBoost Ensemble co RMSE/R2 tot nhat nhe, nhung Tuned Ensemble co MAPE nho nhat va duoc registry chon.
- Gia thue phu thuoc manh vao tinh trang noi that, thoi han hop dong, dich vu, tien ich, dieu ma du lieu hien tai chua co cau truc day du.

### 6.5 Ket qua EDA noi bat

Theo du lieu main:

- Khu vuc nhieu tin nhat la **Thanh pho Thu Duc** voi 4,602 tin.
- Tiep theo la **Quan 7** voi 3,125 tin va **Quan Binh Thanh** voi 2,505 tin.
- Loai BDS nhieu nhat la **Nha pho** voi 6,478 tin, tiep theo la **Phong/Cho thue** voi 5,882 tin.
- Phan khuc ban chiem 49.2%, cho thue chiem 50.8%.
- Gia ban trung vi la 6.30 ty VND; gia thue trung vi la 25 trieu VND/thang.
- 48.9% dong co toa do goc hop le; phan con lai duoc canh bao/uoc luong khi len ban do.

### 6.6 Ket qua suy luan thong ke trong dashboard

Dashboard khong luu ket qua t-test/bootstrap co dinh vi nguoi dung co the chon giao dich, loai BDS, khu vuc A/B, co mau va so lan lap. Tuy nhien, logic thong ke nhu sau:

#### Xac suat thuc nghiem

```text
P(A) = so dong thoa A / tong so dong
```

Vi du cac bang trong app tinh:

- `P(khu vực = district A)`
- `P(loại BDS = category)`
- `P(giá/m2 >= Q3)`
- `P(category | district)`
- `P(tọa độ gốc từ nguồn)`

#### ECDF

```text
F_n(x) = (1/n) * sum(I(X_i <= x))
```

ECDF giup doc percentile truc tiep. Neu duong ECDF cua Quan 1 nam lech phai hon Quan Binh Tan, dieu do cho thay gia/m2 tai Quan 1 cao hon tren hau het cac percentile.

#### CLT simulation

Ung dung lay mau co hoan lai nhieu lan tu `price_per_m2`, moi lan tinh trung binh mau. Khi co mau tang, phan phoi trung binh mau co xu huong tien gan dang chuan:

```text
Xbar_n approx Normal(mu, sigma^2/n)
```

#### Bootstrap CI cho trung vi

Quy trinh:

1. Lay mau co hoan lai kich thuoc n tu mau goc.
2. Tinh trung vi mau bootstrap.
3. Lap B lan, mac dinh trong UI la 600 va co the thay doi.
4. Lay quantile theo muc tin cay 90%, 95% hoac 99%.

Bootstrap phu hop vi trung vi ben vung hon trung binh voi gia BDS, nhung khong co cong thuc sai so chuan don gian nhu mean.

#### Welch's t-test

Gia thuyet:

```text
H0: mean(log(gia/m2)) cua hai khu vuc bang nhau
H1: mean(log(gia/m2)) cua hai khu vuc khac nhau
```

Thong ke:

```text
t = (mean_A - mean_B) / sqrt(s_A^2/n_A + s_B^2/n_B)
```

Neu p-value < 0.05, dashboard ket luan bac bo H0.

#### Wilcoxon Rank-Sum

Wilcoxon khong doi hoi phan phoi chuan, phu hop khi du lieu lech va co outlier. Dashboard chay song song voi t-test de co ket qua ben hon.

### 6.7 Thao luan sai so va han che

Cac ly do khien sai so du doan con cao:

1. **Gia dang tin khac gia giao dich**: nguoi ban co the dang cao hon gia chot de de thuong luong.
2. **Thieu dac trung vi mo**: huong nha, mat tien duong bao nhieu met, hem xe tai hay xe may, phap ly chi tiet, nam xay, chat luong noi that, view, tang, tien ich.
3. **Nhan loai BDS chua hoan toan dong nhat**: `Can ho`, `Can ho/Chung cu`, `Can ho chung cu` co the can gom thanh taxonomy chuan hon.
4. **Outlier gia rat lon**: cac tin nha pho Quan 1, biet thu, mat bang kinh doanh co gia cuc cao lam RMSE lon.
5. **Bias nguon du lieu**: Mogi chiem 63.1% va Cho Tot chiem 26.8% so dong, co the anh huong phan phoi chung.
6. **Toa do thieu/uoc luong**: 15,449 dong khong co toa do goc hop le, nen app uoc luong theo tam khu vuc.
7. **Data leakage da duoc giam nhung can tiep tuc kiem soat**: target encoding trong training da fit tren train, nhung cac bien tong hop trong featured CSV van can duoc ra soat neu mo rong pipeline.

### 6.8 Diem manh cua cach tiep can

- Du an co pipeline end-to-end, khong chi la notebook phan tich tinh.
- Co tach raw/interim/main, giup truy vet du lieu.
- Co app Shiny de nguoi dung thao tac truc tiep.
- Co model registry va metrics CSV de tai su dung trong UI.
- Co check script cho app/model va tro ly.
- Co chatbot local, khong phu thuoc API ngoai.
- Co data quality cards, khong che dau loi du lieu.

---

## 7. KET LUAN (CONCLUSIONS)

### 7.1 Ket qua dat duoc

Do an da xay dung duoc mot he thong phan tich bat dong san TP.HCM tuong doi hoan chinh bang R:

- Thu thap/import du lieu tu 6 nguon.
- Chuan hoa thanh tap du lieu chinh 30,250 dong, 56 cot.
- Xay dung pipeline ETL co the chay lai.
- Tao dac trung tu van ban, thoi gian, khong gian va gia khu vuc.
- Sinh 8 bieu do EDA offline va nhieu bieu do tuong tac.
- Tich hop suy luan thong ke truc tiep tren dashboard.
- Train/evaluate 5 mo hinh hoi quy cho moi phan khuc ban/thue.
- Luu model RDS, metrics CSV, registry CSV, importance CSV.
- Trien khai Shiny Dashboard nhieu tab.
- Xay dung tro ly BDS local co intent detection, entity extraction, memory hoi thoai, listing cards va du doan gia.

### 7.2 Gia tri hoc thuat

De tai the hien duoc nhieu noi dung mon Lap trinh R cho phan tich:

- Cau truc du an R co module.
- Doc/ghi CSV, SQLite cache, RDS artifact.
- Data cleaning voi `dplyr`, `readr`, `stringr`, `lubridate`.
- Web scraping/API voi `httr`, `jsonlite`, `rvest`, `xml2`.
- Truc quan hoa voi `ggplot2`, `plotly`, `leaflet`.
- Thong ke mo ta, xac suat thuc nghiem, ECDF, bootstrap, CLT, kiem dinh gia thuyet.
- Hoi quy tuyen tinh, Random Forest, XGBoost, Ensemble, K-Means.
- Shiny reactive programming.
- Kiem thu smoke test bang script.

### 7.3 Han che

- Du lieu la listing price, chua phai transaction price.
- MAPE con cao, nhat la phan khuc ban.
- Mot so category chua duoc gom nhom thanh taxonomy duy nhat.
- Chua co thong tin anh, mo ta dai, tien ich, phap ly chi tiet, do rong duong/hem.
- Mot so nguon co so dong it, khien ket qua theo nguon/khu vuc co the kem on dinh.
- Co mot so dong thieu toa do goc, nhung app da uoc luong bang tam khu vuc va gan nhan minh bach.
- Chua co RMarkdown/Word/PPTX trong repo; file markdown nay la nen de chuyen sang bao cao nop mon.

### 7.4 Huong phat trien

1. Chuan hoa taxonomy loai BDS: gom `Can ho`, `Can ho/Chung cu`, `Can ho chung cu`; gom cac nhan mat bang/van phong/phong tro ro rang.
2. Them feature tu mo ta dai va anh: NLP tren description, computer vision cho chat luong noi that.
3. Them bien vi tri chi tiet: khoang cach den metro, truong hoc, benh vien, trung tam thuong mai, song/cong vien.
4. Bo sung du lieu giao dich thuc neu co nguon hop phap.
5. Dung spatial model hoac geohash encoding thay vi chi Haversine den Quan 1.
6. Dung cross-validation theo thoi gian/nguon de danh gia do ben.
7. Cai tien mo hinh bang LightGBM/CatBoost neu moi truong R cho phep.
8. Them calibration/uncertainty interval cho gia du doan.
9. Tao RMarkdown tu file nay va render Word/PDF.
10. Tao slide PPTX tom tat dung cac bieu do trong `plots/`.

---

## 8. PHU LUC (APPENDICES)

### 8.1 Lenh chay ung dung

```bash
Rscript app.R
```

Mac dinh:

```text
host = 127.0.0.1
port = 3838
```

Co the cau hinh bang bien moi truong:

```bash
BDS_APP_HOST=127.0.0.1 BDS_APP_PORT=3838 Rscript app.R
```

`app.R` co ham giai phong port neu port dang bi process cu giu.

### 8.2 Lenh chay pipeline day du

```bash
Rscript scripts/pipeline/chay_pipeline.R
```

Pipeline day du gom 12 buoc:

1. Scrape Cho Tot.
2. Scrape Alonhadat.
3. Import Alonhadat local CSV.
4. Scrape Luachonnhadat.
5. Scrape Muaban.
6. Scrape bo sung Mogi, uu tien tin cho thue.
7. Import Mogi tu CSV goc/bo sung/crawl.
8. Import Homedy.
9. Gop du lieu nhieu nguon.
10. Feature engineering.
11. Tao EDA plots.
12. Train models.

### 8.3 Lenh cap nhat nhanh du lieu

```bash
Rscript scripts/pipeline/cap_nhat_du_lieu.R
```

Script nay:

- Cap nhat raw data.
- Gop nguon.
- Tao lai featured CSV.
- Khong retrain model.
- Ghi log vao `data/logs/nhat_ky_cap_nhat.csv`.

Khi can keo du lieu den mot moc so dong cu the, van dung chung file pipeline nay thay vi tao script rieng:

```bash
UPDATE_TO_TARGET=1 TARGET_ROWS=30000 DRY_RUN=1 Rscript scripts/pipeline/cap_nhat_du_lieu.R
UPDATE_TO_TARGET=1 TARGET_ROWS=30000 Rscript scripts/pipeline/cap_nhat_du_lieu.R
```

Che do target in ke hoach truoc voi `DRY_RUN=1`; khi chay that, script uu tien profile Mogi cho thue, co ho tro crawl incremental bang `MOGI_START_PAGE` va `MOGI_APPEND_EXISTING`, sau do gop nguon va tao lai featured CSV nhu luong cap nhat chinh.

Log hien tai:

| Thoi diem | Trang thai | Before | After | New rows |
|---|---|---:|---:|---:|
| 2026-05-16 23:22:45 | success | 3,210 | 3,171 | -39 |
| 2026-06-03 14:08:38 | success | 4,322 | 4,322 | 0 |
| 2026-06-18 23:29:57 | success | 16,209 | 20,856 | 4,647 |
| 2026-06-19 00:24:18 | success | 20,856 | 24,210 | 3,354 |
| 2026-06-19 00:46:21 | success | 24,210 | 29,810 | 5,600 |
| 2026-06-19 00:49:20 | success | 29,810 | 30,250 | 440 |

### 8.4 Lenh cap nhat tu dong va retrain co dieu kien

```bash
Rscript scripts/pipeline/tu_dong_cap_nhat.R
```

Retrain neu:

- Chua co metadata model.
- Lan dau co dataset.
- Ty le dong moi >= `RETRAIN_MIN_NEW_RATIO`, mac dinh 0.12.
- Model cu hon `RETRAIN_MAX_MODEL_AGE_DAYS`, mac dinh 7 ngay.
- `FORCE_RETRAIN=1`.

Log hien tai trong `data/logs/nhat_ky_tu_dong_cap_nhat.csv` co mot lan success ngay 2026-05-16, khong retrain vi `data_refreshed_model_still_valid`.

### 8.5 Lenh chay tung buoc

```bash
Rscript scripts/processing/gop_nguon_du_lieu.R
Rscript scripts/processing/tao_dac_trung.R
Rscript scripts/analysis/phan_tich_eda.R
Rscript scripts/models/huan_luyen_mo_hinh.R
```

### 8.6 Lenh kiem tra du an

```bash
Rscript scripts/checks/kiem_tra_du_an.R
```

Script kiem tra:

- File featured CSV ton tai.
- Model ban va thue ton tai.
- Metrics va registry ton tai.
- App load duoc.
- Du lieu co du dong demo.
- Du doan mau cho phan khuc ban/cho thue tra ve gia hop le.

### 8.7 Lenh kiem tra tro ly

```bash
Rscript scripts/checks/kiem_tra_tro_ly.R
```

Script kiem tra:

- Intent `scout`, `compare`, `undervalued`, `predict`, `recommend`.
- Trich ngan sach 4 ty, dien tich 60/70m2, so phong 2PN.
- Trich khu vuc Thu Duc, Quan 7, Binh Tan, Tan Phu.
- Memory follow-up giu context cu va them khu vuc moi.
- HTML response khong rong.
- Listing request render listing cards.

### 8.8 Artifact dau ra

| Artifact | Noi dung |
|---|---|
| `data/interim/du_lieu_gop_nguon.csv` | Du lieu sau gop nguon va loc co ban. |
| `data/main/du_lieu_chinh.csv` | Du lieu chinh cho app/model/EDA. |
| `plots/*.png` | Bieu do EDA va model. |
| `plots/tom_tat_eda_hcm.csv` | Tong hop district-category. |
| `models/mo_hinh_gia_ban.rds` | Bundle model ban. |
| `models/mo_hinh_gia_thue.rds` | Bundle model cho thue. |
| `models/chi_so_mo_hinh.csv` | Tat ca metric validate. |
| `models/dang_ky_mo_hinh.csv` | Best model theo phan khuc. |
| `models/do_quan_trong_bien_ban.csv` | Feature importance ban. |
| `models/do_quan_trong_bien_thue.csv` | Feature importance thue. |
| `models/cum_gia_quan_huyen.csv` | Ket qua K-Means. |
| `models/thong_tin_mo_hinh.rds` | Metadata lan train. |

### 8.9 Thu vien R su dung

| Nhom | Package |
|---|---|
| Xu ly du lieu | `dplyr`, `readr`, `purrr`, `stringr`, `lubridate`, `tibble` |
| Web/API | `httr`, `jsonlite`, `rvest`, `xml2` |
| Song song/cache | `furrr`, `future`, `DBI`, `RSQLite` |
| Truc quan hoa | `ggplot2`, `plotly`, `leaflet`, `DT` |
| Machine learning | `randomForest`, `xgboost`, `Matrix` |
| Ung dung | `shiny`, `htmltools` thong qua Shiny ecosystem |
| Tuy chon Muaban | `chromote` neu can render Chrome headless |

### 8.10 Cau truc Shiny module

| Module | Noi dung |
|---|---|
| UI | Sidebar, topbar, tab hidden panel, input/output placeholder. |
| Server | Reactive data, filter scope, render chart/map/table, prediction, chat. |
| Helpers | Load data/model, format VND, target encoding prediction, assistant NLP, report PDF. |
| CSS | Layout, mau sac, cards, filters, chat UI, responsive. |
| JS | Navigation active state, resize Plotly, auto-scroll chat, STT/TTS. |

### 8.11 Diagram trong `docs/diagrams/`

| File | Noi dung |
|---|---|
| `kien_truc_he_thong.mmd` | Kien truc nguon du lieu -> storage -> analytics -> outputs. |
| `luong_du_lieu.mmd` | Luong scrape/import -> schema -> merge -> feature engineering -> main CSV. |
| `schema_du_lieu.mmd` | ERD LISTINGS va FEATURED_LISTINGS. |
| `quy_trinh_mo_hinh_ml.mmd` | Train/test, Linear, RF, XGBoost, Ensemble, metrics, model artifacts. |
| `dieu_huong_ung_dung.mmd` | Dieu huong ung dung Shiny. |

Luu y: mot so diagram co the can cap nhat lai cho trung khop 100% voi app hien tai, vi app da co them tab Suy luan thong ke, Danh gia model va Tro ly BDS.

---

## 9. DONG GOP (CONTRIBUTIONS)

Phan nay co the dung de viet vao bao cao nhom. Repo khong chua ten thanh vien, nen bang duoi trinh bay theo module cong viec; khi nop bao cao can thay `Thanh vien 1/2/3/4` bang ho ten that.

| Hang muc | Noi dung dong gop | File/minh chung |
|---|---|---|
| Thu thap du lieu | Xay scraper/importer cho Cho Tot, Alonhadat, Luachonnhadat, Muaban, Mogi, Homedy | `scripts/scrapers/`, `scripts/importers/` |
| Chuan hoa du lieu | Thiet ke schema chung, khu trung lap, loc gia/dien tich, chuan hoa quan/huyen | `gop_nguon_du_lieu.R`, `chuan_hoa_quan_huyen.R` |
| Feature engineering | Regex title, Haversine, log transform, thoi gian, price segment | `tao_dac_trung.R` |
| EDA | Tao plots offline va dashboard phan tich gia | `phan_tich_eda.R`, `may_chu_ung_dung.R` |
| Suy luan thong ke | Xac suat, ECDF, CLT, bootstrap, t-test, Wilcoxon | Tab `statistics` trong app |
| Machine learning | Linear, RF, XGBoost, Ensemble, K-Means, metrics, registry | `huan_luyen_mo_hinh.R`, `models/` |
| Shiny dashboard | UI, server, filter, map, prediction, diagnostics, data table | `app.R`, `ung_dung/` |
| Tro ly BDS | Intent detection, entity extraction, memory, local responses, listing cards | `ham_ho_tro_ung_dung.R`, `kiem_tra_tro_ly.R` |
| Giao dien | CSS dashboard, chat UI, responsive, JS nav/STT/TTS | `www/giao_dien.css`, `www/tuong_tac.js` |
| Tai lieu | README, data README, diagram, bao cao co so ly thuyet | `README.md`, `data/README.md`, `docs/` |

Bang phan cong goi y:

| Thanh vien | Nhiem vu chinh | San pham can minh chung | Muc do hoan thanh |
|---|---|---|---|
| Thanh vien 1 | Thu thap/import du lieu va pipeline | Raw CSV, scraper/importer, log update | Can dien |
| Thanh vien 2 | Lam sach, feature engineering, EDA | Main CSV, plots, EDA dashboard | Can dien |
| Thanh vien 3 | Model ML, K-Means, metrics | RDS model, metrics CSV, registry | Can dien |
| Thanh vien 4 | Shiny UI/server, tro ly, bao cao | Dashboard, assistant, docs/report | Can dien |

---

## 10. THAM KHAO (REFERENCES)

### 10.1 Tai lieu de bai va tai lieu trong du an

1. `docs/references/do_an_ket_thuc_mon_hoc_R.pdf`: de cuong do an ket thuc mon hoc, goi y cau truc bao cao.
2. `README.md`: tong quan du an, cau truc thu muc, lenh chay.
3. `data/README.md`: giai thich vong doi du lieu trong thu muc `data/`.
4. `docs/diagrams/*.mmd`: cac so do kien truc, luong du lieu, schema va model.

### 10.2 Tai lieu package va phuong phap

1. R Project: ngon ngu va moi truong tinh toan thong ke.
2. Shiny: xay dung ung dung web tuong tac bang R.
3. ggplot2: grammar of graphics cho truc quan hoa.
4. plotly: bieu do tuong tac.
5. leaflet: ban do tuong tac.
6. dplyr/readr/stringr/lubridate: xu ly du lieu dang bang, CSV, chuoi va thoi gian.
7. rvest/xml2/httr/jsonlite: thu thap va parse du lieu web/API.
8. randomForest: Random Forest Regression.
9. xgboost: Gradient Boosting Regression.
10. Matrix: sparse matrix cho model matrix cua XGBoost.
11. DT: bang du lieu tuong tac trong Shiny.
12. DBI/RSQLite: cache SQLite cho scraper Cho Tot.

### 10.3 Cong thuc/phuong phap thong ke su dung

1. Log transformation `log1p`.
2. Haversine distance.
3. Target encoding co smoothing.
4. Linear Regression/OLS.
5. Random Forest/Bagging.
6. XGBoost/Gradient Boosting.
7. Weighted Ensemble.
8. K-Means clustering.
9. MAE, RMSE, MAPE, R2.
10. Empirical probability va conditional probability.
11. ECDF.
12. Central Limit Theorem simulation.
13. Percentile Bootstrap confidence interval.
14. Welch's t-test.
15. Wilcoxon Rank-Sum test.

---

## 11. PEER ASSESSMENT

Phan Peer assessment theo yeu cau de bai can nhom hop va danh gia muc do dong gop, uu diem, han che cua tung thanh vien. Vi repo khong co thong tin ho ten, ben duoi la khung danh gia de dien truc tiep.

### 11.1 Tieu chi danh gia

| Tieu chi | Mo ta | Goi y diem |
|---|---|---:|
| Muc do hoan thanh nhiem vu | Hoan thanh dung phan cong, co san pham chay duoc | 0-10 |
| Chat luong ky thuat | Code ro rang, co module, it loi, dung du lieu that | 0-10 |
| Tinh chu dong | Tu tim cach xu ly van de, chu dong fix bug/cap nhat | 0-10 |
| Hop tac nhom | Giao tiep, ho tro thanh vien khac, tich hop cong viec | 0-10 |
| Tai lieu va trinh bay | Ghi chu, viet bao cao, giai thich duoc phan minh lam | 0-10 |

### 11.2 Bang danh gia mau

| Thanh vien | Phan viec | Diem manh | Han che | Diem de xuat |
|---|---|---|---|---:|
| Thanh vien 1 | Thu thap du lieu/pipeline | Can dien | Can dien | Can dien |
| Thanh vien 2 | Lam sach/EDA | Can dien | Can dien | Can dien |
| Thanh vien 3 | Model/thong ke | Can dien | Can dien | Can dien |
| Thanh vien 4 | Shiny/tro ly/bao cao | Can dien | Can dien | Can dien |

### 11.3 Nhan xet tong ket nhom goi y

Nhom da hoan thanh duoc mot he thong tuong doi day du tu du lieu den ung dung. Uu diem la san pham co pipeline, co dashboard tuong tac, co model artifact, co data quality va co tro ly local. Han che la du lieu van la gia dang tin, mo hinh con sai so cao, mot so nhan loai BDS chua gom nhom tuyet doi va chua co du thong tin ngoai sinh nhu tien ich, phap ly chi tiet, mat tien duong, huong nha. Neu tiep tuc phat trien, nhom nen uu tien chuan hoa taxonomy loai BDS, bo sung feature vi tri/tien ich, cai tien validation va xuat bao cao RMarkdown/Word/PPTX hoan chinh.

---

## PHU LUC NHANH: DAN Y DOI SANG BAO CAO WORD/RMARKDOWN

Khi chuyen file nay sang bao cao nop mon, co the giu cau truc:

1. Tom tat.
2. Gioi thieu.
3. Du lieu.
4. Truc quan hoa du lieu.
5. Mo hinh hoa du lieu.
6. Thuc nghiem, ket qua va thao luan.
7. Ket luan.
8. Phu luc.
9. Dong gop.
10. Tham khao.
11. Peer assessment.

Neu can rut gon cho Word, uu tien giu cac bang:

- Bang nguon du lieu.
- Bang schema/dac trung.
- Bang chat luong du lieu.
- Bang metric model.
- Bang feature importance.
- Bang dong gop/peer assessment.
