# Data Layout

Thu muc `data/` duoc chia theo vong doi du lieu:

```text
data/
|-- main/       # Dataset chinh app/model dang dung
|-- interim/    # Dataset da gop va chuan hoa trung gian
|-- raw/        # Du lieu rieng tung nguon
|-- cache/      # Cache ky thuat, co the tao lai
`-- logs/       # Log cap nhat pipeline
```

## Main

`main/du_lieu_chinh.csv` la dataset chinh cua dashboard Shiny va cac model du doan.

## Interim

`interim/du_lieu_gop_nguon.csv` la dataset da gop tu tat ca raw source ve mot schema chung, truoc feature engineering.

## Raw

Moi nguon co thu muc rieng:

| Thu muc | Y nghia |
|---|---|
| `raw/chotot/` | Raw CSV va schema Chotot sau scraper |
| `raw/alonhadat/` | Raw Alonhadat va CSV local da chuan hoa |
| `raw/luachonnhadat/` | Raw/clean Luachonnhadat |
| `raw/muaban/` | Raw Muaban |
| `raw/mogi/` | CSV Mogi goc, clean va standardized |
| `raw/homedy/` | CSV Homedy goc va standardized |

## Cache Va Logs

`cache/cache_chotot.sqlite` la cache SQLite cua scraper Chotot.

`logs/*.csv` la lich su chay update/auto-update.
