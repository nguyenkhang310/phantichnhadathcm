# Cơ Sở Lý Thuyết Và Công Nghệ Ứng Dụng Trong Đồ Án Phân Tích Giá Bất Động Sản TP.HCM

## 1. Tổng Quan Đề Tài

Đồ án xây dựng hệ thống phân tích và dự đoán giá bất động sản tại Thành phố Hồ Chí Minh bằng ngôn ngữ R. Hệ thống thu thập dữ liệu tin đăng từ nhiều nguồn, chuẩn hóa dữ liệu về cùng một cấu trúc, xử lý dữ liệu, tạo đặc trưng, phân tích khám phá, huấn luyện mô hình học máy và triển khai kết quả thông qua dashboard Shiny.

Mục tiêu chính của đồ án gồm:

- Thu thập dữ liệu bất động sản từ nhiều nguồn trực tuyến như Chợ Tốt, Alonhadat, Lựa Chọn Nhà Đất, Mua Bán, Mogi và Homedy.
- Chuẩn hóa dữ liệu từ các nguồn khác nhau về cùng một schema.
- Phân tích các yếu tố ảnh hưởng đến giá bất động sản như vị trí, diện tích, loại hình, số phòng, đặc điểm tiêu đề, thời gian đăng tin và khoảng cách tới trung tâm.
- Áp dụng xác suất thống kê và suy luận thống kê để phân tích mặt bằng giá, xác suất có điều kiện, phân phối mẫu, khoảng tin cậy bootstrap và kiểm định giả thuyết.
- Xây dựng mô hình dự đoán giá bán và giá thuê bất động sản.
- Đánh giá mô hình bằng các chỉ số RMSE, MAE, MAPE và R².
- Trực quan hóa dữ liệu, mô hình và kết quả dự đoán bằng ứng dụng web Shiny.

Về bản chất, đồ án là một bài toán khoa học dữ liệu ứng dụng trong lĩnh vực bất động sản. Quy trình tổng quát gồm các bước:

1. Thu thập dữ liệu.
2. Làm sạch và chuẩn hóa dữ liệu.
3. Tạo đặc trưng phục vụ phân tích và mô hình hóa.
4. Phân tích khám phá dữ liệu.
5. Áp dụng xác suất thống kê, bootstrap và kiểm định giả thuyết.
6. Huấn luyện và đánh giá mô hình học máy.
7. Triển khai kết quả trên dashboard tương tác.

## 2. Cơ Sở Lý Thuyết Về Bất Động Sản

### 2.1. Khái Niệm Bất Động Sản

Bất động sản là tài sản gắn liền với đất đai, bao gồm đất, nhà ở, căn hộ, công trình xây dựng và các tài sản khác không thể di dời hoặc gắn liền lâu dài với vị trí địa lý cụ thể. Trong đồ án này, bất động sản được hiểu theo nghĩa thực tế trên thị trường tin đăng, bao gồm các loại hình như căn hộ, nhà ở, nhà phố, đất, phòng trọ, văn phòng và mặt bằng kinh doanh.

Dữ liệu bất động sản thường có các thuộc tính quan trọng:

- Giá: tổng giá bán hoặc giá thuê của bất động sản.
- Diện tích: diện tích sử dụng hoặc diện tích đất, thường tính bằng mét vuông.
- Vị trí: quận, huyện, phường, xã, địa chỉ, vĩ độ và kinh độ.
- Loại hình: căn hộ, nhà ở, đất, phòng trọ, văn phòng, mặt bằng.
- Số phòng: số phòng ngủ hoặc số phòng sử dụng.
- Thời gian đăng tin: ngày, giờ hoặc thời điểm tin được đăng.
- Mô tả tin đăng: tiêu đề, nội dung mô tả, thông tin pháp lý, nội thất, mặt tiền, hẻm xe hơi.

### 2.2. Đặc Điểm Thị Trường Bất Động Sản

Thị trường bất động sản có một số đặc điểm khác biệt so với nhiều loại hàng hóa thông thường:

- Tính vị trí: bất động sản gắn với một vị trí cố định, do đó giá chịu ảnh hưởng mạnh bởi khu vực, hạ tầng, tiện ích, giao thông và mức độ phát triển đô thị.
- Tính không đồng nhất: mỗi bất động sản có đặc điểm riêng về diện tích, pháp lý, hướng, mặt tiền, số tầng, chất lượng xây dựng và môi trường xung quanh.
- Giá trị lớn: giao dịch bất động sản thường có giá trị cao, khiến sai số dự đoán cần được đánh giá cẩn thận.
- Thanh khoản không đồng đều: một số khu vực hoặc phân khúc có lượng giao dịch cao, trong khi một số khu vực ít dữ liệu hơn.
- Thông tin thị trường phân tán: dữ liệu thường nằm rải rác trên nhiều website, mỗi nguồn có cách biểu diễn thông tin khác nhau.

Các đặc điểm này dẫn đến nhu cầu sử dụng phương pháp phân tích dữ liệu và học máy để tổng hợp, chuẩn hóa và khai thác thông tin một cách có hệ thống.

### 2.3. Giá Bất Động Sản Và Giá Trên Mét Vuông

Trong đồ án, hai đại lượng quan trọng là giá tổng và giá trên mét vuông.

Giá tổng là giá trị được người đăng tin đưa ra cho toàn bộ bất động sản:

```text
price = tổng giá bán hoặc tổng giá thuê
```

Giá trên mét vuông giúp so sánh các bất động sản có diện tích khác nhau:

```text
price_per_m2 = price / area
```

Trong đó:

- `price` là giá bất động sản.
- `area` là diện tích bất động sản.
- `price_per_m2` là giá trên một mét vuông.

Giá trên mét vuông đặc biệt hữu ích khi phân tích mặt bằng giá giữa các quận, loại hình bất động sản hoặc cụm khu vực. Ví dụ, một căn nhà giá 10 tỷ đồng chưa chắc đắt hơn một căn hộ giá 5 tỷ đồng nếu diện tích của căn nhà lớn hơn nhiều. Giá trên mét vuông giúp chuẩn hóa sự khác biệt về diện tích.

### 2.4. Phân Biệt Giá Bán Và Giá Thuê

Đồ án tách riêng hai phân khúc:

- Bán: giá thường ở mức hàng trăm triệu đến hàng trăm tỷ đồng.
- Cho thuê: giá thường ở mức hàng trăm nghìn đến hàng trăm triệu đồng mỗi kỳ thuê.

Hai phân khúc này có thang đo giá, hành vi thị trường và yếu tố ảnh hưởng khác nhau. Vì vậy, hệ thống huấn luyện riêng mô hình cho dữ liệu bán và dữ liệu cho thuê:

- `models/mo_hinh_gia_ban.rds` cho phân khúc bán.
- `models/mo_hinh_gia_thue.rds` cho phân khúc cho thuê.

Việc tách mô hình giúp hạn chế hiện tượng một mô hình duy nhất phải học hai phân phối giá quá khác nhau.

## 3. Cơ Sở Lý Thuyết Về Khoa Học Dữ Liệu

### 3.1. Khoa Học Dữ Liệu

Khoa học dữ liệu là lĩnh vực kết hợp thống kê, lập trình, khai phá dữ liệu, trực quan hóa và học máy để rút ra tri thức từ dữ liệu. Trong đồ án này, khoa học dữ liệu được áp dụng để trả lời các câu hỏi:

- Khu vực nào có nhiều tin đăng bất động sản?
- Loại hình bất động sản nào có giá cao hơn?
- Giá trên mét vuông thay đổi như thế nào theo quận?
- Diện tích có quan hệ như thế nào với giá?
- Các đặc điểm trong tiêu đề tin đăng có ảnh hưởng đến giá không?
- Mô hình nào dự đoán giá tốt hơn?

Một quy trình khoa học dữ liệu cơ bản gồm:

1. Xác định bài toán.
2. Thu thập dữ liệu.
3. Làm sạch dữ liệu.
4. Khám phá dữ liệu.
5. Tạo đặc trưng.
6. Xây dựng mô hình.
7. Đánh giá mô hình.
8. Triển khai và trực quan hóa kết quả.

### 3.2. Dữ Liệu Có Cấu Trúc

Dữ liệu trong đồ án được lưu chủ yếu dưới dạng bảng CSV và SQLite. Mỗi dòng biểu diễn một tin đăng bất động sản, mỗi cột biểu diễn một thuộc tính của tin đăng. Đây là dạng dữ liệu có cấu trúc, phù hợp với các thao tác xử lý bằng R và các thư viện như `dplyr`, `readr`, `DBI`, `RSQLite`.

Các cột dữ liệu chính gồm:

- `source`: nguồn dữ liệu.
- `source_id`: mã định danh tin đăng theo nguồn.
- `title`: tiêu đề tin đăng.
- `price`: giá.
- `area`: diện tích.
- `rooms`: số phòng.
- `address`: địa chỉ.
- `ward`: phường hoặc xã.
- `district_name`: quận hoặc huyện.
- `category_name`: loại hình bất động sản.
- `lat`, `lon`: tọa độ địa lý.
- `posted_at`: thời điểm đăng tin.
- `is_rent`: phân biệt bán và cho thuê.

### 3.3. ETL Trong Xử Lý Dữ Liệu

ETL là viết tắt của Extract, Transform, Load:

- Extract: trích xuất dữ liệu từ nguồn.
- Transform: làm sạch, chuẩn hóa và biến đổi dữ liệu.
- Load: lưu dữ liệu đã xử lý vào nơi sử dụng tiếp theo.

Trong đồ án:

- Extract: các scraper lấy dữ liệu từ API, HTML hoặc CSV.
- Transform: các script chuẩn hóa cột, lọc dữ liệu lỗi, tạo đặc trưng.
- Load: dữ liệu được lưu vào CSV, SQLite, file RDS và hiển thị trên Shiny dashboard.

Quy trình ETL giúp hệ thống có thể xử lý nhiều nguồn dữ liệu khác nhau nhưng vẫn đảm bảo dữ liệu đầu ra có cấu trúc thống nhất.

## 4. Thu Thập Dữ Liệu

### 4.1. Web Scraping

Web scraping là kỹ thuật tự động thu thập dữ liệu từ website. Dữ liệu có thể được lấy từ:

- API trả về JSON.
- Trang HTML cần phân tích cấu trúc DOM.
- File CSV có sẵn.

Trong đồ án, web scraping được thực hiện bằng các package R:

- `httr`: gửi HTTP request tới API hoặc website.
- `jsonlite`: phân tích dữ liệu JSON.
- `rvest` và `xml2`: đọc và trích xuất dữ liệu từ HTML.
- `purrr`, `furrr`, `future`: hỗ trợ lặp, xử lý song song hoặc xử lý nhiều trang.
- `lubridate`: xử lý thời gian.

### 4.2. Thu Thập Qua API

API là giao diện cho phép chương trình truy vấn dữ liệu từ máy chủ. Khi website cung cấp endpoint API, dữ liệu thường được trả về ở định dạng JSON. Thu thập qua API có ưu điểm là dữ liệu có cấu trúc rõ ràng, dễ đọc và ít phụ thuộc vào giao diện HTML.

Trong đồ án, dữ liệu Chợ Tốt được lấy từ endpoint API và xử lý bằng `httr` kết hợp `jsonlite`. Các trường như mã tin, tiêu đề, giá, diện tích, tọa độ, danh mục và thời gian đăng tin được ánh xạ thành bảng dữ liệu.

### 4.3. Thu Thập Qua HTML

Với các website không cung cấp API thuận tiện, hệ thống đọc HTML và trích xuất thông tin theo selector. Đây là cách áp dụng cho một số nguồn như Alonhadat và Mua Bán.

Quy trình cơ bản:

1. Gửi request tới URL danh sách tin.
2. Đọc nội dung HTML.
3. Xác định các node chứa tin đăng.
4. Trích xuất tiêu đề, giá, diện tích, địa chỉ, đường dẫn.
5. Chuyển dữ liệu thành bảng.

Hạn chế của scraping HTML là cấu trúc website có thể thay đổi, làm selector không còn đúng. Do đó scraper cần có cơ chế xử lý lỗi, retry và chuẩn hóa sau khi thu thập.

### 4.4. Import CSV

Một số dữ liệu có thể đã được làm sạch hoặc thu thập riêng dưới dạng CSV. Trong đồ án, nguồn Mogi và Homedy được import từ CSV đã làm sạch. Đây là cách tích hợp dữ liệu đơn giản và ổn định hơn so với scraping trực tiếp.

### 4.5. Chuẩn Hóa Nhiều Nguồn Dữ Liệu

Mỗi website có thể đặt tên cột khác nhau, đơn vị giá khác nhau, định dạng ngày khác nhau hoặc thiếu một số trường. Vì vậy, đồ án dùng bước gộp nguồn trong `scripts/processing/gop_nguon_du_lieu.R` để đưa tất cả dữ liệu về cùng một schema chuẩn.

Schema chuẩn gồm các nhóm thông tin:

- Thông tin nguồn: `source`, `source_group`, `source_id`, `ad_id`.
- Thông tin tin đăng: `title`, `ad_url`, `source_url`, `image`.
- Thông tin giá và diện tích: `price`, `price_str`, `area`, `rooms`, `price_m`, `price_per_m2`.
- Thông tin vị trí: `address`, `ward`, `district_id`, `district_name`, `lat`, `lon`.
- Thông tin loại hình: `category_id`, `category_name`, `is_rent`.
- Thông tin thời gian: `posted_at`, `scraped_at`, `page_fetched`.

Việc chuẩn hóa schema giúp các bước sau như EDA, feature engineering và train model không phải xử lý riêng từng nguồn.

## 5. Làm Sạch Dữ Liệu

### 5.1. Khái Niệm Làm Sạch Dữ Liệu

Làm sạch dữ liệu là quá trình phát hiện và xử lý dữ liệu bị thiếu, sai định dạng, trùng lặp, bất thường hoặc không phù hợp với bài toán. Dữ liệu tin đăng bất động sản thường chứa nhiều nhiễu vì được nhập bởi người dùng hoặc lấy từ nhiều website.

Các vấn đề thường gặp:

- Giá bị thiếu hoặc bằng 0.
- Diện tích không hợp lý.
- Quận, phường ghi không thống nhất.
- Một tin đăng xuất hiện nhiều lần.
- Tọa độ nằm ngoài TP.HCM.
- Dữ liệu bán và cho thuê bị lẫn thang giá.
- Tiêu đề chứa nhiều cách viết tắt.

### 5.2. Lọc Giá Bất Hợp Lý

Trong đồ án, dữ liệu được lọc theo khoảng giá phù hợp:

- Tin cho thuê: từ 300.000 VND đến 2 tỷ VND.
- Tin bán: từ 300 triệu VND đến 500 tỷ VND.

Cách lọc này giúp loại bỏ các tin có giá nhập sai, giá tượng trưng hoặc không phù hợp với bài toán dự đoán. Đây là bước quan trọng vì các giá trị ngoại lai quá lớn hoặc quá nhỏ có thể làm mô hình học sai xu hướng.

### 5.3. Lọc Diện Tích Bất Hợp Lý

Diện tích được lọc trong khoảng hợp lý, ví dụ từ 5 m² đến 5000 m². Những diện tích quá nhỏ hoặc quá lớn có thể là lỗi nhập liệu hoặc không thuộc phạm vi bất động sản dân dụng/thương mại thông thường.

### 5.4. Xử Lý Dữ Liệu Trùng Lặp

Dữ liệu từ cùng một nguồn hoặc nhiều nguồn có thể bị trùng. Đồ án dùng `source_id` làm định danh để loại bỏ các dòng trùng:

```text
distinct(source_id, .keep_all = TRUE)
```

Việc loại bỏ trùng lặp giúp tránh việc một tin đăng xuất hiện nhiều lần làm sai lệch thống kê và ảnh hưởng đến mô hình.

### 5.5. Chuẩn Hóa Tên Quận Huyện

Tên quận huyện có thể được ghi theo nhiều cách:

- `Q.1`, `Quận 1`, `quan 1`.
- `Thủ Đức`, `TP Thủ Đức`, `Thành phố Thủ Đức`.
- `Bình Thạnh`, `Quan Binh Thanh`, `Q.Bình Thạnh`.

Đồ án sử dụng script `scripts/lib/chuan_hoa_quan_huyen.R` để chuẩn hóa tên quận huyện. Chuẩn hóa địa danh giúp nhóm dữ liệu chính xác hơn khi thống kê, trực quan hóa hoặc train model.

### 5.6. Xử Lý Nhãn Thiếu

Các giá trị như `unknown`, `NA`, chuỗi rỗng, `Không rõ` được xem là nhãn thiếu. Trong dashboard và pipeline, hệ thống chuyển các nhãn này về dạng hiển thị thống nhất, thường là `Không rõ`.

### 5.7. Xử Lý Tọa Độ

Tọa độ vĩ độ và kinh độ được kiểm tra bằng bounding box của TP.HCM:

```text
10.30 <= lat <= 11.20
106.00 <= lon <= 107.30
```

Nếu tọa độ nằm ngoài vùng hợp lý, hệ thống xem là không hợp lệ. Khi thiếu tọa độ, dashboard có thể ước lượng vị trí theo trung tâm quận/huyện để phục vụ hiển thị bản đồ.

## 6. Feature Engineering

### 6.1. Khái Niệm Feature Engineering

Feature engineering là quá trình tạo ra các biến đặc trưng mới từ dữ liệu ban đầu nhằm giúp mô hình học máy hiểu dữ liệu tốt hơn. Trong bất động sản, giá không chỉ phụ thuộc vào diện tích và quận, mà còn phụ thuộc vào nhiều thông tin ẩn trong tiêu đề, địa chỉ, loại hình và thời gian đăng tin.

Script `scripts/processing/tao_dac_trung.R` đảm nhiệm bước này.

### 6.2. Biến Giá Trên Mét Vuông

Biến `price_per_m2` được tính:

```text
price_per_m2 = price / area
```

Biến này dùng cho:

- So sánh mặt bằng giá giữa các quận.
- Phân tích loại hình bất động sản.
- Phân cụm khu vực bằng K-Means.

### 6.3. Biến Log Giá

Giá bất động sản thường có phân phối lệch phải, nghĩa là phần lớn tin có giá vừa phải nhưng một số tin có giá rất cao. Để giảm ảnh hưởng của giá trị cực đoan, đồ án dùng biến log:

```text
log_price = log(1 + price)
```

Khi dự đoán xong, giá được chuyển ngược về thang đo ban đầu:

```text
price = exp(log_price) - 1
```

Lợi ích của log transform:

- Giảm độ lệch của phân phối giá.
- Giúp mô hình hồi quy ổn định hơn.
- Hạn chế ảnh hưởng của outlier.
- Biến sai số tương đối thành dạng dễ học hơn.

### 6.4. Biến Log Diện Tích

Tương tự giá, diện tích cũng có thể lệch phải. Biến `log_area` được tính:

```text
log_area = log(1 + area)
```

Biến này giúp mô hình học quan hệ phi tuyến giữa diện tích và giá.

### 6.5. Khoảng Cách Tới Trung Tâm

Vị trí là yếu tố quan trọng trong định giá bất động sản. Đồ án tính khoảng cách từ tọa độ tin đăng tới một điểm trung tâm TP.HCM bằng công thức Haversine.

Công thức Haversine dùng để tính khoảng cách giữa hai điểm trên bề mặt Trái Đất:

```text
a = sin²(delta_lat / 2) + cos(lat1) * cos(lat2) * sin²(delta_lon / 2)
c = 2 * atan2(sqrt(a), sqrt(1 - a))
d = R * c
```

Trong đó:

- `lat1`, `lon1` là tọa độ bất động sản.
- `lat2`, `lon2` là tọa độ trung tâm.
- `R` là bán kính Trái Đất, xấp xỉ 6371 km.
- `d` là khoảng cách tính bằng km.

Trong đồ án, điểm trung tâm được dùng là khoảng:

```text
lat = 10.7758
lon = 106.7009
```

Biến `distance_to_center` phản ánh mức độ gần trung tâm, có thể ảnh hưởng đến giá.

### 6.6. Đặc Trưng Từ Tiêu Đề Tin Đăng

Tiêu đề tin đăng thường chứa nhiều thông tin quan trọng nhưng không nằm trong cột riêng. Đồ án trích xuất các đặc trưng từ tiêu đề và địa chỉ:

- `frontage_width_m`: chiều ngang nhà/đất, lấy từ mẫu dạng `4x15`.
- `frontage_length_m`: chiều dài nhà/đất.
- `frontage_ratio`: tỷ lệ ngang/dài.
- `inferred_floors`: số tầng/lầu suy luận từ chữ như `3 tầng`, `1 trệt 2 lầu`.
- `inferred_rooms`: số phòng ngủ suy luận từ chữ như `2PN`, `3 phòng ngủ`.
- `title_has_frontage`: có thông tin mặt tiền.
- `title_has_alley`: có thông tin hẻm/ngõ.
- `title_has_car_access`: có hẻm xe hơi hoặc ô tô vào được.
- `title_has_corner`: căn góc hoặc hai mặt tiền.
- `title_has_elevator`: có thang máy.
- `title_has_furnished`: có nội thất.
- `title_has_legal`: có pháp lý, sổ hồng, giấy tờ.
- `title_has_income_info`: có thông tin hợp đồng, dòng tiền, đang cho thuê.
- `title_token_count`: độ dài hoặc mức độ chi tiết của tiêu đề.

Các đặc trưng này giúp mô hình khai thác thông tin định tính từ văn bản ngắn.

### 6.7. Đặc Trưng Thời Gian

Thời điểm đăng tin được chuyển thành các biến:

- `posted_hour`: giờ đăng tin.
- `posted_wday`: thứ trong tuần.
- `is_weekend_post`: tin có đăng vào cuối tuần hay không.
- `listing_age_days`: số ngày từ lúc đăng tin đến thời điểm xử lý.

Các biến thời gian có thể phản ánh hành vi đăng tin và độ mới của tin.

### 6.8. Target Encoding Theo Phường Xã

Target encoding là kỹ thuật mã hóa biến phân loại bằng thống kê của biến mục tiêu. Trong đồ án, phường/xã được mã hóa bằng mặt bằng giá trung vị.

Công thức smoothing được dùng trong quá trình train:

```text
encoded_value = log(1 + (n * median_price_group + smoothing * global_median) / (n + smoothing))
```

Trong đó:

- `n` là số tin trong phường/xã.
- `median_price_group` là giá trung vị của phường/xã.
- `global_median` là giá trung vị toàn bộ tập train.
- `smoothing` giúp tránh việc nhóm ít dữ liệu bị nhiễu quá mạnh.

Kỹ thuật này giúp mô hình sử dụng thông tin vị trí chi tiết hơn so với chỉ dùng quận/huyện.

### 6.9. Xử Lý Thiếu Bằng Trung Vị Và Mode

Một số biến bị thiếu được thay thế bằng thống kê phù hợp:

- Diện tích thiếu: thay bằng trung vị theo nhóm quận và loại hình, nếu không có thì dùng trung vị toàn cục.
- Số phòng thiếu: thay bằng mode theo loại hình.
- Các đặc trưng số suy luận thiếu: thay bằng trung vị hoặc 0.
- Khoảng cách thiếu: thay bằng trung vị khoảng cách.

Cách xử lý này giúp dữ liệu đủ điều kiện đưa vào mô hình mà không loại bỏ quá nhiều dòng.

## 7. Phân Tích Khám Phá Dữ Liệu

### 7.1. Khái Niệm EDA

EDA, hay Exploratory Data Analysis, là quá trình phân tích khám phá dữ liệu nhằm hiểu cấu trúc, phân phối, xu hướng, ngoại lệ và mối quan hệ giữa các biến. EDA thường được thực hiện trước khi xây dựng mô hình.

Trong đồ án, EDA được thực hiện bởi `scripts/analysis/phan_tich_eda.R` và xuất ra các biểu đồ trong thư mục `plots/`.

### 7.2. Phân Phối Giá

Biểu đồ histogram của `log(1 + price)` giúp quan sát phân phối giá sau khi biến đổi log. Nếu phân phối giá gốc bị lệch mạnh, biểu đồ log thường dễ đọc và ổn định hơn.

Ý nghĩa:

- Xác định khoảng giá phổ biến.
- Phát hiện nhóm tin bất thường.
- Kiểm tra độ phù hợp của log transform.

### 7.3. Boxplot Giá Theo Quận

Boxplot thể hiện phân phối giá theo từng quận/huyện. Mỗi boxplot gồm trung vị, khoảng tứ phân vị và điểm ngoại lai.

Ý nghĩa:

- So sánh mặt bằng giá giữa các quận.
- Phát hiện khu vực có giá cao hoặc biến động lớn.
- Nhận biết outlier trong từng khu vực.

### 7.4. Quan Hệ Giữa Diện Tích Và Giá

Scatter plot giữa diện tích và giá giúp quan sát mối quan hệ giữa quy mô bất động sản và giá. Thông thường, diện tích càng lớn thì giá tổng càng cao, nhưng mức tăng không nhất thiết tuyến tính vì còn phụ thuộc vị trí và loại hình.

### 7.5. Top Khu Vực Theo Số Lượng Tin

Biểu đồ cột top 10 quận có nhiều tin đăng nhất giúp đánh giá mức độ phổ biến của khu vực trong dữ liệu. Khu vực có nhiều tin hơn thường cung cấp mẫu tốt hơn cho mô hình.

### 7.6. Top Khu Vực Theo Giá Trên Mét Vuông

Biểu đồ top 10 quận có giá/m² cao nhất giúp nhận diện các khu vực có mặt bằng giá cao. Đây là thông tin có giá trị trong phân tích thị trường.

### 7.7. Xu Hướng Tin Đăng Theo Ngày

Biểu đồ đường số lượng tin đăng theo ngày giúp theo dõi biến động dữ liệu theo thời gian, phát hiện các giai đoạn thu thập mạnh hoặc bất thường.

### 7.8. Phân Bố Địa Lý

Biểu đồ địa lý theo vĩ độ, kinh độ cho thấy phân bố tin đăng trên không gian TP.HCM. Khi kết hợp màu theo giá, biểu đồ giúp quan sát vùng có giá cao/thấp.

### 7.9. Ma Trận Tương Quan

Ma trận tương quan giúp đo mức độ liên hệ tuyến tính giữa các biến số. Hệ số tương quan Pearson giữa hai biến `X` và `Y`:

```text
Corr(X, Y) = Cov(X, Y) / (SD(X) * SD(Y))
```

Trong đó:

```text
Cov(X, Y) = (1/(n - 1)) * sum((x_i - x_bar) * (y_i - y_bar))
```

Ý nghĩa:

- `Corr` gần 1: hai biến có xu hướng tăng cùng nhau.
- `Corr` gần -1: một biến tăng thì biến kia có xu hướng giảm.
- `Corr` gần 0: quan hệ tuyến tính yếu.

Trong đồ án, ma trận tương quan giúp xem quan hệ giữa giá, diện tích, giá/m², số phòng, khoảng cách tới trung tâm và các đặc trưng số khác. Tuy nhiên, tương quan chỉ phản ánh quan hệ tuyến tính và không chứng minh quan hệ nhân quả.

### 7.10. Heatmap Khu Vực - Loại Hình

Heatmap dùng màu sắc để thể hiện độ lớn của một chỉ số theo hai chiều phân nhóm, ví dụ quận/huyện và loại bất động sản.

Một ô heatmap có thể biểu diễn:

```text
median_price_per_m2(district, category)
```

hoặc:

```text
listing_count(district, category)
```

Ý nghĩa:

- Nhận diện khu vực nào có mặt bằng giá/m² cao theo từng loại hình.
- Phát hiện loại hình nào xuất hiện nhiều ở từng quận/huyện.
- Hỗ trợ giải thích vì sao mô hình cần tương tác `district_category_price_encoded`.

### 7.11. Treemap Và Sunburst

Treemap và sunburst biểu diễn dữ liệu phân cấp, ví dụ:

```text
Nguồn dữ liệu -> Loại giao dịch -> Loại bất động sản
```

Tỷ trọng của một nhóm:

```text
share_group = count_group / total_count
```

Ý nghĩa:

- Xem nguồn nào đóng góp nhiều dữ liệu.
- Xem cấu trúc dữ liệu có lệch về loại hình nào không.
- Giúp giải thích khả năng model học tốt hơn ở nhóm nhiều dữ liệu và kém ổn định hơn ở nhóm ít dữ liệu.

### 7.12. Xu Hướng Thời Gian Và Đường Trung Bình

Với dữ liệu có ngày đăng tin, có thể gom theo ngày hoặc tuần:

```text
count_t = số tin đăng tại thời điểm t
median_price_t = median(price tại thời điểm t)
```

Đường xu hướng giúp quan sát:

- Giai đoạn dữ liệu được thu thập nhiều hay ít.
- Mặt bằng giá có biến động theo thời gian không.
- Có ngày bất thường do nguồn dữ liệu thay đổi, lỗi crawl hoặc chiến dịch đăng tin hàng loạt không.

Nếu dùng trung bình trượt:

```text
moving_average_t = (x_t + x_(t-1) + ... + x_(t-k+1)) / k
```

Trung bình trượt làm đường xu hướng mượt hơn và giảm nhiễu ngắn hạn.

## 8. Cơ Sở Lý Thuyết Về Học Máy

### 8.1. Học Máy

Học máy là lĩnh vực cho phép máy tính học quy luật từ dữ liệu để đưa ra dự đoán hoặc quyết định mà không cần lập trình thủ công toàn bộ quy tắc. Trong đồ án, học máy được dùng để dự đoán giá bất động sản dựa trên các đặc trưng như diện tích, khu vực, loại hình, số phòng, khoảng cách tới trung tâm và đặc điểm tiêu đề.

### 8.2. Học Có Giám Sát

Bài toán dự đoán giá là bài toán học có giám sát vì dữ liệu huấn luyện có biến mục tiêu `price` hoặc `log_price`. Mô hình học mối quan hệ:

```text
X -> y
```

Trong đó:

- `X` là ma trận đặc trưng.
- `y` là giá bất động sản.

Sau khi học từ dữ liệu lịch sử, mô hình có thể dự đoán giá cho một bất động sản mới.

### 8.3. Bài Toán Hồi Quy

Vì giá là biến liên tục, bài toán thuộc nhóm hồi quy. Mục tiêu của hồi quy là học một hàm dự đoán:

```text
f: X -> y
```

Trong đó:

- `X` là tập đặc trưng mô tả bất động sản.
- `y` là biến mục tiêu.
- `f(X)` là giá trị mô hình dự đoán.

Với từng tin đăng thứ `i`, dữ liệu có dạng:

```text
D = {(x_1, y_1), (x_2, y_2), ..., (x_n, y_n)}
```

Trong đồ án:

```text
y_i = log(1 + price_i)
```

Mô hình không dự đoán trực tiếp `price` mà dự đoán `log_price`. Sau khi có dự đoán trên thang log, hệ thống chuyển ngược về giá VND:

```text
predicted_price = exp(predicted_log_price) - 1
```

Lý do dùng `log_price`:

- Giá bất động sản thường lệch phải rất mạnh: đa số tin ở mức vừa phải, một số ít tin rất đắt.
- Log transform làm phân phối ổn định hơn, giảm ảnh hưởng quá lớn của các giá trị cực trị.
- Quan hệ giữa diện tích, vị trí, loại hình và giá thường có tính nhân: ví dụ nhà mặt tiền hoặc khu trung tâm có thể làm giá tăng theo tỷ lệ. Khi lấy log, quan hệ nhân được chuyển gần hơn về quan hệ cộng.
- Dự đoán trên log rồi lấy `exp()` giúp giá dự đoán luôn dương.
- Sai số trên thang log gần với sai số tương đối, phù hợp với thị trường bất động sản vì lệch 500 triệu ở căn 2 tỷ khác ý nghĩa với lệch 500 triệu ở căn 50 tỷ.

Vector đặc trưng `x_i` trong đồ án gồm các nhóm chính:

- Đặc trưng quy mô: `area`, `log_area`, `rooms`, số tầng/phòng suy luận.
- Đặc trưng vị trí: quận/huyện, phường/xã, khoảng cách tới trung tâm.
- Đặc trưng loại hình: căn hộ, nhà phố, đất nền, kho xưởng, phòng trọ...
- Đặc trưng nguồn dữ liệu: nguồn tin đăng.
- Đặc trưng thời gian: giờ đăng, thứ đăng, tuổi tin đăng.
- Đặc trưng văn bản tiêu đề: mặt tiền, hẻm xe hơi, thang máy, pháp lý, nội thất, dòng tiền.
- Đặc trưng mã hóa giá theo nhóm: mặt bằng giá theo phường, quận, loại hình, nguồn và tổ hợp quận - loại hình.

Các mô hình hồi quy được sử dụng:

- Linear Regression.
- Random Forest Regression.
- XGBoost Regression.
- RF + XGBoost Ensemble.
- Tuned RF/XGBoost Ensemble.

### 8.4. Chia Train/Test

Để đánh giá mô hình, dữ liệu được chia thành hai phần:

- Tập train: dùng để học tham số, học quy luật và fit các bước xử lý phụ thuộc dữ liệu.
- Tập test/holdout: dùng để kiểm tra mô hình trên dữ liệu chưa được dùng để huấn luyện.

Nếu vừa huấn luyện vừa đánh giá trên cùng một tập dữ liệu, mô hình có thể đạt điểm rất đẹp nhưng không phản ánh khả năng dự đoán dữ liệu mới. Vì vậy, train/test split là bước bắt buộc để ước lượng khả năng tổng quát hóa.

Đồ án dùng tỷ lệ 80/20:

```text
train = 80% dữ liệu
test = 20% dữ liệu
```

Trong code, dữ liệu được chia riêng cho hai phân khúc:

- `sale`: dữ liệu bán.
- `rent`: dữ liệu cho thuê.

Theo kết quả train hiện tại trong `models/dang_ky_mo_hinh.csv`:

```text
sale: train = 8,218 dòng, test = 2,056 dòng
rent: train = 4,745 dòng, test = 1,188 dòng
```

Lý do chọn tỷ lệ 80/20:

- 80% train đủ lớn để các mô hình nhiều tham số như Random Forest và XGBoost học được quan hệ giữa khu vực, loại hình, diện tích, nguồn tin và giá.
- 20% test vẫn đủ nhiều để tính RMSE, MAE, MAPE và R² tương đối ổn định.
- Nếu dùng 70/30, tập train nhỏ hơn, mô hình có thể mất dữ liệu học, nhất là ở các nhóm ít mẫu như một số phường hoặc loại hình hiếm.
- Nếu dùng 90/10, tập test nhỏ hơn, chỉ số đánh giá dễ dao động do vài tin bất thường.
- 80/20 là tỷ lệ phổ biến trong các đồ án học máy khi dữ liệu ở mức vài nghìn đến vài chục nghìn dòng và chưa triển khai cross-validation đầy đủ.

Việc chia dữ liệu được stratified theo nguồn dữ liệu khi có đủ mẫu:

```text
Mỗi nguồn dữ liệu được chia xấp xỉ 80% train và 20% test.
```

Ý nghĩa của stratified split:

- Tránh trường hợp train có nhiều tin từ một nguồn nhưng test lại thiếu nguồn đó.
- Giữ phân phối nguồn dữ liệu giữa train và test tương đối giống nhau.
- Đánh giá công bằng hơn vì mỗi nguồn có cách đăng tin, cách ghi giá, chất lượng dữ liệu và phân khúc khác nhau.

Đồ án dùng `set.seed(42)` để kết quả chia dữ liệu có thể tái lập. Khi một nhóm nguồn có quá ít mẫu, code dùng cơ chế fallback ngẫu nhiên để vẫn đảm bảo có dữ liệu train/test hợp lệ.

Một điểm quan trọng là các bước target encoding được fit trên tập train, sau đó mới áp dụng sang test. Điều này giúp tránh data leakage, tức là tránh việc mô hình nhìn thấy thông tin giá của tập test trong quá trình huấn luyện.

Sau khi chọn được mô hình tốt nhất bằng tập holdout, code refit mô hình cuối trên toàn bộ dữ liệu sạch của từng phân khúc. Cách làm này giúp tận dụng tối đa dữ liệu khi lưu mô hình phục vụ dashboard. Tuy nhiên, các chỉ số báo cáo vẫn lấy từ lần đánh giá trên tập holdout để phản ánh khả năng dự đoán dữ liệu chưa thấy.

### 8.5. Overfitting Và Underfitting

Overfitting xảy ra khi mô hình học quá sát dữ liệu train, bao gồm cả nhiễu, dẫn đến dự đoán kém trên dữ liệu mới.

Underfitting xảy ra khi mô hình quá đơn giản, không học được quy luật quan trọng trong dữ liệu.

Có thể hiểu theo quan hệ bias - variance:

- Underfitting thường có bias cao: mô hình quá đơn giản nên sai cả trên train và test.
- Overfitting thường có variance cao: mô hình rất tốt trên train nhưng kém trên test.

Trong đồ án, các biện pháp giảm rủi ro overfitting gồm:

- Đánh giá trên tập holdout 20%.
- So sánh mô hình đơn giản và mô hình phức tạp.
- Dùng nhiều chỉ số: RMSE, MAE, MAPE, R².
- Với XGBoost, thử nhiều cấu hình và kiểm soát độ phức tạp bằng `max_depth`, `min_child_weight`, `subsample`, `colsample_bytree`.
- Với Random Forest, dùng nhiều cây và lấy trung bình để giảm phương sai.
- Giới hạn dự đoán log trong khoảng phân vị 1% - 99% của train để tránh dự đoán quá cực đoan.

## 9. Linear Regression

### 9.1. Khái Niệm

Linear Regression là mô hình hồi quy tuyến tính, giả định biến mục tiêu có quan hệ tuyến tính với các biến đầu vào:

```text
y = beta0 + beta1*x1 + beta2*x2 + ... + betap*xp + epsilon
```

Trong đó:

- `y` là biến mục tiêu.
- `x1, x2, ..., xp` là các đặc trưng.
- `beta0` là hệ số chặn.
- `beta1, ..., betap` là hệ số hồi quy.
- `epsilon` là sai số.

Trong đồ án, `y` là `log_price`.

Với `n` dòng dữ liệu và `p` đặc trưng, mục tiêu của Linear Regression là tìm bộ hệ số làm tổng bình phương sai số nhỏ nhất:

```text
minimize sum((y_i - y_hat_i)^2), i = 1..n
```

Trong đó:

```text
y_hat_i = beta0 + beta1*x_i1 + beta2*x_i2 + ... + betap*x_ip
```

Ở dạng ma trận:

```text
y = X * beta + epsilon
```

Nếu ma trận `X'X` khả nghịch, nghiệm OLS có dạng:

```text
beta_hat = (X'X)^(-1) X'y
```

Vì biến mục tiêu là `log_price`, một hệ số `beta_j` có thể hiểu gần đúng theo tỷ lệ:

```text
price thay đổi khoảng (exp(beta_j) - 1) * 100%
```

khi đặc trưng `x_j` tăng 1 đơn vị và các biến khác giữ nguyên. Cách diễn giải này chỉ nên dùng thận trọng vì dữ liệu bất động sản có nhiều biến tương quan nhau.

### 9.2. Giả Định Lý Thuyết

Các giả định kinh điển của hồi quy tuyến tính gồm:

- Quan hệ giữa biến đầu vào và biến mục tiêu gần tuyến tính sau khi biến đổi phù hợp.
- Sai số có kỳ vọng bằng 0.
- Phương sai sai số tương đối ổn định.
- Các quan sát tương đối độc lập.
- Không có đa cộng tuyến quá mạnh giữa các biến giải thích.

Trong đồ án, Linear Regression chủ yếu đóng vai trò baseline. Vì dữ liệu bất động sản có quan hệ phi tuyến mạnh, ta không kỳ vọng Linear Regression luôn là mô hình tốt nhất, nhưng nó giúp trả lời câu hỏi: mô hình phức tạp có thật sự cải thiện so với cách tuyến tính đơn giản hay không?

### 9.3. Ưu Điểm

- Dễ hiểu và dễ giải thích.
- Huấn luyện nhanh.
- Phù hợp làm mô hình baseline.
- Có thể cho biết chiều tác động của từng biến nếu dữ liệu phù hợp.

### 9.4. Hạn Chế

- Khó học quan hệ phi tuyến phức tạp.
- Nhạy cảm với ngoại lai.
- Cần giả định tương đối tuyến tính giữa đặc trưng và biến mục tiêu.
- Dễ bị ảnh hưởng khi các biến phân loại có nhiều mức hoặc khi các biến tương quan mạnh.

### 9.5. Ứng Dụng Trong Đồ Án

Trong đồ án, Linear Regression được dùng như mô hình cơ sở để so sánh với các mô hình mạnh hơn. Nếu mô hình phi tuyến như Random Forest hoặc XGBoost không cải thiện nhiều, Linear Regression vẫn là lựa chọn có tính giải thích tốt.

Quy trình trong code:

1. Tạo công thức `log_price ~ feature_1 + feature_2 + ...`.
2. Loại các biến không hợp lệ hoặc factor chỉ có một mức trong tập train.
3. Fit mô hình bằng `lm()`.
4. Dự đoán `log_price` trên test.
5. Giới hạn dự đoán trong khoảng log giá hợp lý.
6. Chuyển về VND bằng `exp(pred) - 1`.
7. Tính RMSE, MAE, MAPE, R².

## 10. Random Forest

### 10.1. Khái Niệm

Random Forest là thuật toán ensemble dựa trên nhiều cây quyết định. Với bài toán hồi quy, mỗi cây đưa ra một giá trị dự đoán, sau đó Random Forest lấy trung bình các dự đoán.

```text
prediction = average(prediction_tree_1, prediction_tree_2, ..., prediction_tree_n)
```

Mỗi cây được huấn luyện trên một mẫu bootstrap của dữ liệu và tại mỗi lần chia nhánh chỉ xét một tập con ngẫu nhiên các biến.

Với `B` cây, dự đoán Random Forest Regression là:

```text
f_RF(x) = (1 / B) * sum(T_b(x)), b = 1..B
```

Trong đó:

- `T_b(x)` là dự đoán của cây thứ `b`.
- `B` là số cây trong rừng.

Một cây quyết định hồi quy chia dữ liệu theo các điều kiện như:

```text
area < 60
district_name = "Quận 1"
category_name = "Nhà phố"
```

Ở mỗi node, cây chọn phép chia giúp giảm sai số trong các node con. Một tiêu chí phổ biến là giảm tổng bình phương sai số:

```text
SSE = sum((y_i - mean(y_node))^2)
```

Random Forest kết hợp hai nguồn ngẫu nhiên:

- Bootstrap sampling: mỗi cây học trên một mẫu lấy lại từ train.
- Random feature selection: tại mỗi node chỉ xét một tập con đặc trưng.

Nhờ vậy, các cây khác nhau không giống hệt nhau. Khi lấy trung bình nhiều cây, sai số ngẫu nhiên giảm xuống.

### 10.2. Ưu Điểm

- Học được quan hệ phi tuyến.
- Ít nhạy cảm hơn với outlier so với hồi quy tuyến tính.
- Hoạt động tốt với dữ liệu có nhiều loại đặc trưng.
- Có thể tính feature importance.
- Giảm overfitting so với một cây quyết định đơn lẻ.

### 10.3. Hạn Chế

- Khó giải thích hơn Linear Regression.
- Dự đoán và huấn luyện có thể tốn tài nguyên hơn.
- Không ngoại suy tốt ngoài phạm vi dữ liệu đã học.

### 10.4. Feature Importance

Random Forest có thể đánh giá mức độ quan trọng của từng đặc trưng. Trong đồ án, độ quan trọng được lưu vào:

- `models/do_quan_trong_bien_ban.csv`
- `models/do_quan_trong_bien_thue.csv`

Feature importance giúp trả lời câu hỏi biến nào ảnh hưởng nhiều đến dự đoán giá, ví dụ diện tích, khu vực, loại hình hay mặt bằng giá phường/xã.

Trong `randomForest`, độ quan trọng có thể hiểu theo mức độ một biến giúp giảm lỗi khi chia cây. Nếu một biến thường xuyên tạo ra các split làm giảm sai số mạnh, biến đó có importance cao. Tuy nhiên, feature importance không đồng nghĩa quan hệ nhân quả. Ví dụ quận/huyện quan trọng không có nghĩa chỉ cần đổi tên quận là giá đổi, mà vì quận đại diện cho vị trí, hạ tầng, tiện ích và mặt bằng thị trường.

### 10.5. Ứng Dụng Trong Đồ Án

Trong `scripts/models/huan_luyen_mo_hinh.R`, Random Forest được train với:

```text
ntree = 500
importance = TRUE
```

`ntree = 500` nghĩa là mô hình huấn luyện 500 cây. Số cây đủ lớn giúp dự đoán ổn định hơn so với một cây đơn lẻ. Sau một mức nhất định, tăng số cây thường cải thiện ít hơn nhưng tốn thời gian hơn, nên 500 là mức cân bằng hợp lý cho đồ án.

Mô hình này vừa phục vụ dự đoán, vừa phục vụ phân tích tầm quan trọng của đặc trưng.

## 11. XGBoost

### 11.1. Khái Niệm

XGBoost là thuật toán gradient boosting mạnh, xây dựng nhiều cây quyết định theo cách tuần tự. Mỗi cây mới cố gắng sửa lỗi của các cây trước đó. Với bài toán hồi quy, XGBoost tối ưu hàm mất mát để giảm sai số dự đoán.

Ý tưởng tổng quát:

```text
model = tree_1 + tree_2 + ... + tree_n
```

Mỗi cây được thêm vào nhằm cải thiện phần sai số còn lại.

Mô hình XGBoost có dạng cộng dồn:

```text
y_hat_i = sum(f_k(x_i)), k = 1..K
```

Trong đó:

- `K` là số cây boosting.
- `f_k` là cây thứ `k`.
- Mỗi cây mới học phần lỗi còn lại của mô hình trước.

### 11.2. Gradient Boosting

Gradient Boosting là kỹ thuật ensemble trong đó mô hình mới được huấn luyện dựa trên gradient của hàm mất mát. Thay vì tạo nhiều mô hình độc lập như Random Forest, boosting tạo mô hình theo chuỗi, mô hình sau phụ thuộc vào sai số của mô hình trước.

Ở vòng lặp thứ `t`, mô hình cập nhật:

```text
y_hat_i^(t) = y_hat_i^(t-1) + eta * f_t(x_i)
```

Trong đó:

- `eta` là learning rate.
- `f_t` là cây mới ở vòng `t`.
- `y_hat_i^(t-1)` là dự đoán trước khi thêm cây mới.

XGBoost tối ưu hàm mục tiêu gồm loss và regularization:

```text
Obj = sum(l(y_i, y_hat_i)) + sum(Omega(f_k))
```

Với hồi quy bình phương sai số:

```text
l(y_i, y_hat_i) = (y_i - y_hat_i)^2
```

Regularization của cây thường được viết:

```text
Omega(f) = gamma*T + (1/2)*lambda*sum(w_j^2)
```

Trong đó:

- `T` là số lá của cây.
- `w_j` là trọng số tại lá thứ `j`.
- `gamma` phạt cây có quá nhiều lá.
- `lambda` phạt trọng số lá quá lớn.

Nhờ phần phạt này, XGBoost kiểm soát độ phức tạp tốt hơn boosting cơ bản.

### 11.3. Ưu Điểm

- Hiệu quả cao trong nhiều bài toán dữ liệu bảng.
- Học tốt quan hệ phi tuyến và tương tác giữa biến.
- Có nhiều tham số để kiểm soát độ phức tạp.
- Thường cho độ chính xác tốt hơn các mô hình đơn giản.

### 11.4. Hạn Chế

- Cần chọn tham số cẩn thận.
- Có thể overfit nếu số vòng lặp hoặc độ sâu cây quá lớn.
- Khó giải thích hơn Linear Regression.

### 11.5. Tham Số Trong Đồ Án

Đồ án thử nhiều cấu hình XGBoost, gồm:

- `nrounds`: số vòng boosting, tức số cây được thêm tuần tự.
- `learning_rate`: tốc độ học. Giá trị nhỏ giúp học chậm và ổn định hơn nhưng cần nhiều vòng hơn.
- `max_depth`: độ sâu tối đa của cây. Cây sâu học được tương tác phức tạp nhưng dễ overfit hơn.
- `min_child_weight`: điều kiện tối thiểu để chia node. Giá trị lớn làm mô hình bảo thủ hơn.
- `subsample`: tỷ lệ mẫu dùng cho mỗi vòng. Dưới 1 giúp giảm overfitting.
- `colsample_bytree`: tỷ lệ biến dùng cho mỗi cây. Dưới 1 giúp giảm phụ thuộc vào một vài biến mạnh.

Trong code, XGBoost dùng:

```text
objective = "reg:squarederror"
```

Tức là tối ưu hồi quy theo sai số bình phương trên thang `log_price`.

Đồ án thử 8 bộ tham số với `nrounds` từ 180 đến 500, `learning_rate` từ 0.025 đến 0.10 và `max_depth` từ 4 đến 7. Mô hình được chọn theo MAPE thấp nhất trên tập holdout, nếu MAPE bằng nhau thì xét RMSE.

Sau khi dự đoán, giá trị `predicted_log_price` được giới hạn trong khoảng phân vị 1% - 99% của `log_price` trong tập train:

```text
lower_bound = quantile(train_log_price, 0.01)
upper_bound = quantile(train_log_price, 0.99)
pred = min(max(pred, lower_bound), upper_bound)
```

Mục đích là tránh dự đoán quá cực đoan so với phạm vi dữ liệu đã học.

### 11.6. Sparse Matrix

XGBoost thường nhận dữ liệu dạng ma trận số. Với biến phân loại như quận, loại hình và thứ đăng tin, đồ án dùng `sparse.model.matrix` để tạo ma trận one-hot encoding dạng sparse.

Ví dụ biến `district_name` có nhiều quận/huyện sẽ được chuyển thành nhiều cột nhị phân:

```text
district_name_Quan_1
district_name_Quan_3
district_name_Quan_Binh_Thanh
...
```

Với one-hot encoding, mỗi dòng thường chỉ có một vài cột bằng 1 và rất nhiều cột bằng 0. Sparse matrix chỉ lưu các giá trị khác 0 nên tiết kiệm bộ nhớ và tăng tốc huấn luyện.

## 12. Ensemble Model

### 12.1. Khái Niệm Ensemble

Ensemble là phương pháp kết hợp nhiều mô hình để tạo dự đoán cuối cùng. Ý tưởng là các mô hình khác nhau có thể học các khía cạnh khác nhau của dữ liệu; khi kết hợp, kết quả có thể ổn định hơn.

Có ba nhóm ensemble phổ biến:

- Bagging: huấn luyện nhiều mô hình độc lập rồi lấy trung bình, ví dụ Random Forest.
- Boosting: huấn luyện mô hình tuần tự, mô hình sau sửa lỗi mô hình trước, ví dụ XGBoost.
- Blending/stacking: kết hợp dự đoán của nhiều mô hình khác nhau.

### 12.2. Ensemble Trong Đồ Án

Đồ án dùng ensemble đơn giản giữa Random Forest và XGBoost:

```text
ensemble_pred = (rf_pred + xgb_pred) / 2
```

Dự đoán cuối cùng là trung bình của hai mô hình. Cách này dễ triển khai, giảm phụ thuộc vào một mô hình duy nhất và thường giúp kết quả ổn định hơn nếu hai mô hình có sai số khác nhau.

Ngoài ensemble trung bình 50/50, đồ án còn dùng tuned ensemble:

```text
ensemble_pred = w * rf_pred + (1 - w) * xgb_pred
```

Trong đó:

- `w` là trọng số dành cho Random Forest.
- `1 - w` là trọng số dành cho XGBoost.
- `w` được thử từ 0 đến 1 với bước 0.05.

Code chọn `w` tạo ra MAPE thấp nhất trên tập holdout. Nếu hai trọng số có MAPE bằng nhau, chọn trọng số có RMSE thấp hơn.

Ý nghĩa:

- Nếu `w = 0.5`, hai mô hình đóng góp ngang nhau.
- Nếu `w > 0.5`, Random Forest được tin cậy hơn.
- Nếu `w < 0.5`, XGBoost được tin cậy hơn.

Ensemble có thể tốt hơn từng mô hình riêng lẻ khi lỗi của hai mô hình không hoàn toàn giống nhau. Random Forest thường ổn định do bagging, XGBoost thường bắt quan hệ phức tạp tốt do boosting. Kết hợp hai cách nhìn này giúp dự đoán cân bằng hơn.

### 12.3. Chọn Mô Hình Tốt Nhất

Sau khi train, đồ án so sánh năm lựa chọn:

- Linear Regression.
- Random Forest.
- XGBoost.
- RF + XGBoost Ensemble.
- Tuned RF/XGBoost Ensemble.

Mô hình tốt nhất được chọn theo MAPE thấp nhất, sau đó xét RMSE. Kết quả được lưu trong:

```text
models/dang_ky_mo_hinh.csv
```

Theo kết quả hiện tại, mô hình tốt nhất cho cả phân khúc bán và thuê là:

```text
Tuned RF/XGBoost Ensemble
```

Điều này hợp lý vì giá bất động sản chịu tác động đồng thời của nhiều yếu tố phi tuyến và tương tác mạnh. Ensemble tận dụng được điểm mạnh của cả Random Forest và XGBoost.

### 12.4. Refit Mô Hình Cuối

Sau khi chọn được thuật toán tốt nhất và tham số tốt nhất, code huấn luyện lại mô hình cuối trên toàn bộ dữ liệu sạch của từng phân khúc.

Lý do refit:

- Giai đoạn đánh giá cần giữ 20% holdout để đo chất lượng.
- Khi đã biết mô hình nào tốt, mô hình dùng trong dashboard nên tận dụng cả 100% dữ liệu sạch để học mặt bằng giá mới nhất.
- Các chỉ số báo cáo vẫn dựa trên holdout, không lấy từ mô hình refit toàn bộ, nên không làm đẹp sai số một cách giả tạo.

## 13. K-Means Clustering

### 13.1. Khái Niệm Phân Cụm

Phân cụm là bài toán học không giám sát, mục tiêu là chia dữ liệu thành các nhóm sao cho các điểm trong cùng nhóm giống nhau hơn so với điểm thuộc nhóm khác.

Trong đồ án, phân cụm dùng để nhóm các tổ hợp khu vực và loại hình bất động sản theo đặc điểm giá/m², diện tích trung vị và số lượng tin đăng.

### 13.2. Thuật Toán K-Means

K-Means chia dữ liệu thành `k` cụm. Thuật toán hoạt động theo các bước:

1. Chọn `k` tâm cụm ban đầu.
2. Gán mỗi điểm dữ liệu vào tâm cụm gần nhất.
3. Cập nhật tâm cụm bằng trung bình các điểm trong cụm.
4. Lặp lại bước 2 và 3 đến khi hội tụ.

Mục tiêu của K-Means là giảm tổng bình phương khoảng cách từ điểm dữ liệu đến tâm cụm:

```text
minimize sum(||x_i - centroid_cluster||²)
```

Viết đầy đủ hơn:

```text
minimize sum_{j=1..k} sum_{x_i in C_j} ||x_i - mu_j||^2
```

Trong đó:

- `C_j` là cụm thứ `j`.
- `mu_j` là tâm cụm thứ `j`.
- `||x_i - mu_j||^2` là bình phương khoảng cách Euclidean từ điểm đến tâm cụm.

### 13.3. Chuẩn Hóa Dữ Liệu Trước Khi Phân Cụm

Các biến như giá/m², diện tích và số lượng tin có thang đo khác nhau. Nếu không chuẩn hóa, biến có giá trị lớn sẽ chi phối khoảng cách. Vì vậy, đồ án dùng `scale()` trước khi chạy K-Means.

Công thức chuẩn hóa z-score:

```text
z = (x - mean(x)) / sd(x)
```

Sau chuẩn hóa, mỗi biến có trung bình gần 0 và độ lệch chuẩn gần 1. Nhờ vậy, `median_price_per_m2`, `median_area` và `listing_count` có vai trò cân bằng hơn khi tính khoảng cách.

### 13.4. Ứng Dụng Trong Đồ Án

Dữ liệu phân cụm gồm:

- `median_price_per_m2`: giá/m² trung vị.
- `median_area`: diện tích trung vị.
- `listing_count`: số lượng tin đăng.

Kết quả được lưu vào:

- `models/mo_hinh_phan_cum_gia_dien_tich.rds`
- `models/cum_gia_quan_huyen.csv`

Phân cụm giúp nhận diện các nhóm thị trường như khu vực giá cao, khu vực diện tích lớn, khu vực có nhiều tin đăng hoặc phân khúc giá thấp hơn.

Trong code, dữ liệu được nhóm theo:

```text
transaction_type + district_name + category_name
```

Sau đó tính:

```text
median_price_per_m2
median_area
listing_count
```

K-Means được chạy riêng theo phân khúc bán/thuê, với số cụm tối đa là 4 và `nstart = 25`. Tham số `nstart = 25` nghĩa là thuật toán thử 25 bộ tâm khởi tạo khác nhau rồi chọn kết quả có tổng sai số trong cụm tốt nhất. Điều này giúp giảm rủi ro kẹt ở nghiệm cục bộ xấu.

## 14. Đánh Giá Mô Hình

### 14.1. RMSE

RMSE, hay Root Mean Squared Error, đo căn bậc hai trung bình bình phương sai số:

```text
RMSE = sqrt((1/n) * sum((actual_i - predicted_i)^2))
```

RMSE phạt mạnh các sai số lớn, phù hợp khi muốn chú ý đến các dự đoán lệch nhiều.

Trong đồ án, RMSE được tính trên thang giá VND sau khi chuyển ngược từ `log_price`. Vì vậy, RMSE có đơn vị là VND. Nếu RMSE cao, nghĩa là tồn tại các dự đoán lệch mạnh, thường xảy ra ở các bất động sản giá rất cao hoặc thông tin thiếu.

### 14.2. MAE

MAE, hay Mean Absolute Error, đo trung bình trị tuyệt đối sai số:

```text
MAE = (1/n) * sum(abs(actual_i - predicted_i))
```

MAE dễ hiểu vì cùng đơn vị với giá. Ví dụ MAE bằng 500 triệu nghĩa là trung bình mô hình lệch khoảng 500 triệu đồng.

So với RMSE, MAE ít bị kéo mạnh bởi các ngoại lệ hơn. Khi trình bày, MAE là chỉ số dễ giải thích nhất với người nghe không chuyên.

### 14.3. MAPE

MAPE, hay Mean Absolute Percentage Error, đo sai số phần trăm tuyệt đối trung bình:

```text
MAPE = (1/n) * sum(abs((actual_i - predicted_i) / actual_i))
```

Nếu muốn biểu diễn dạng phần trăm:

```text
MAPE_percent = MAPE * 100%
```

MAPE giúp so sánh sai số tương đối giữa các mức giá khác nhau. Ví dụ MAPE 0.10 nghĩa là dự đoán trung bình lệch khoảng 10% so với giá thực tế.

Trong đồ án, MAPE được dùng làm tiêu chí chính để chọn mô hình tốt nhất.

Lý do chọn MAPE làm chỉ số chính:

- Giá bất động sản có biên độ rất rộng, từ phòng trọ vài triệu/tháng đến nhà bán hàng chục tỷ.
- Sai số tuyệt đối cùng một mức tiền không có ý nghĩa giống nhau ở các phân khúc khác nhau.
- MAPE trả lời câu hỏi dễ hiểu: mô hình lệch trung bình bao nhiêu phần trăm so với giá đăng.

Hạn chế của MAPE:

- Không phù hợp nếu `actual` gần 0.
- Có thể phạt mạnh các trường hợp giá thấp.

Trong đồ án, giá bất động sản luôn dương và đã được lọc ngưỡng bất hợp lý, nên MAPE vẫn phù hợp để làm chỉ số chính.

### 14.4. R Bình Phương

R bình phương đo tỷ lệ biến thiên của biến mục tiêu được mô hình giải thích:

```text
R² = 1 - SS_res / SS_tot
```

Trong đó:

- `SS_res` là tổng bình phương sai số.
- `SS_tot` là tổng bình phương độ lệch so với trung bình.

Công thức đầy đủ:

```text
SS_res = sum((actual_i - predicted_i)^2)
SS_tot = sum((actual_i - mean(actual))^2)
R² = 1 - SS_res / SS_tot
```

R² càng gần 1 thì mô hình giải thích dữ liệu càng tốt. Tuy nhiên, với dữ liệu bất động sản nhiều nhiễu, R² chỉ nên xem cùng với RMSE, MAE và MAPE.

Diễn giải:

- `R² = 1`: dự đoán khớp hoàn toàn.
- `R² = 0`: mô hình không tốt hơn dự đoán bằng giá trung bình.
- `R² < 0`: mô hình tệ hơn việc luôn dự đoán trung bình.

Với dữ liệu thị trường thực tế, R² không nhất thiết phải cực cao vì nhiều yếu tố quan trọng không có trong dữ liệu, như chất lượng nhà, pháp lý chi tiết, hướng, đường trước nhà, quy hoạch và thương lượng thực tế.

### 14.5. Actual Vs Predicted

Biểu đồ actual vs predicted so sánh giá thực tế và giá dự đoán. Nếu mô hình tốt, các điểm sẽ nằm gần đường chéo `y = x`.

Đồ án xuất biểu đồ:

- `plots/du_doan_so_voi_thuc_te_ban.png`
- `plots/du_doan_so_voi_thuc_te_thue.png`

Biểu đồ này giúp phát hiện mô hình có xu hướng dự đoán thấp ở giá cao hoặc dự đoán cao ở giá thấp hay không.

### 14.6. Residual Analysis

Residual là sai số dự đoán:

```text
residual_i = actual_i - predicted_i
```

Trên thang log:

```text
residual_log_i = log(actual_i + 1) - log(predicted_i + 1)
```

Histogram residual giúp xem sai số có tập trung quanh 0 hay không. Nếu residual lệch hẳn sang dương, mô hình thường dự đoán thấp hơn thực tế. Nếu residual lệch hẳn sang âm, mô hình thường dự đoán cao hơn thực tế.

Biểu đồ residual theo nhóm khu vực giúp phát hiện mô hình sai nhiều ở những quận/huyện nào. Đây là phần rất quan trọng khi phản biện vì mô hình bất động sản thường không sai đều nhau trên toàn thị trường.

### 14.7. So Sánh Metric Giữa Các Model

Bảng/biểu đồ so sánh model trả lời ba câu hỏi:

- Mô hình nào chính xác nhất theo MAPE?
- Mô hình nào ít lỗi lớn nhất theo RMSE?
- Mô hình nào dễ giải thích nhất?

Không nên chỉ nhìn một chỉ số duy nhất. Ví dụ:

- MAPE thấp nhưng RMSE cao: mô hình khá ổn trung bình nhưng vẫn có vài ca lệch rất lớn.
- RMSE thấp nhưng MAPE cao: mô hình tốt ở tin giá cao nhưng có thể lệch tỷ lệ ở tin giá thấp.
- R² cao nhưng MAPE chưa tốt: mô hình giải thích xu hướng chung tốt nhưng dự đoán từng tin vẫn còn sai tương đối lớn.

## 15. Trực Quan Hóa Dữ Liệu

### 15.1. Vai Trò Của Trực Quan Hóa

Trực quan hóa giúp chuyển dữ liệu số thành hình ảnh dễ hiểu. Với bài toán bất động sản, trực quan hóa giúp người dùng nhanh chóng nắm được xu hướng thị trường, sự khác biệt giữa khu vực và hiệu quả mô hình.

### 15.2. ggplot2

`ggplot2` là package trực quan hóa dữ liệu phổ biến trong R, dựa trên Grammar of Graphics. Một biểu đồ trong ggplot2 thường gồm:

- Data: dữ liệu đầu vào.
- Mapping: ánh xạ biến vào trục, màu, kích thước.
- Geom: loại hình biểu diễn như điểm, cột, đường, boxplot.
- Scale: thang đo.
- Theme: giao diện biểu đồ.

Trong đồ án, ggplot2 dùng để tạo histogram, boxplot, scatter plot, bar chart và biểu đồ feature importance.

### 15.3. plotly

`plotly` giúp chuyển biểu đồ thành dạng tương tác, cho phép hover, zoom, pan và xem thông tin chi tiết. Trong dashboard, plotly giúp người dùng khám phá dữ liệu linh hoạt hơn so với biểu đồ tĩnh.

### 15.4. leaflet

`leaflet` là thư viện bản đồ tương tác. Trong đồ án, leaflet dùng để hiển thị tin đăng bất động sản trên bản đồ theo tọa độ hoặc vị trí ước lượng theo khu vực.

Bản đồ giúp trả lời các câu hỏi:

- Tin đăng tập trung ở khu vực nào?
- Khu vực nào có giá cao hơn?
- Phân bố dữ liệu có lệch về một số quận không?

### 15.5. DT

`DT` dùng để hiển thị bảng dữ liệu tương tác trong Shiny, hỗ trợ tìm kiếm, sắp xếp, phân trang và xem chi tiết. Đây là thành phần phù hợp để người dùng kiểm tra danh sách tin đăng sau khi lọc.

## 16. Shiny Dashboard

### 16.1. Khái Niệm Shiny

Shiny là framework của R dùng để xây dựng ứng dụng web tương tác. Shiny cho phép kết hợp giao diện người dùng và xử lý dữ liệu R trong cùng một ứng dụng.

Một ứng dụng Shiny thường gồm:

- UI: định nghĩa giao diện, input, layout, biểu đồ, bảng.
- Server: định nghĩa logic xử lý, phản ứng với input và trả output.

Trong đồ án, file `app.R` chứa cả UI và server của dashboard.

### 16.2. Reactive Programming

Shiny sử dụng lập trình phản ứng. Khi người dùng thay đổi bộ lọc, các thành phần phụ thuộc sẽ tự động cập nhật. Ví dụ:

- Chọn quận khác làm biểu đồ cập nhật.
- Đổi loại giao dịch bán/cho thuê làm bảng và bản đồ thay đổi.
- Nhập diện tích và số phòng làm kết quả dự đoán cập nhật.

### 16.3. Các Thành Phần Dashboard

Dashboard của đồ án gồm các nhóm chức năng:

- Tổng quan dữ liệu và chỉ số chính.
- Phân tích giá theo khu vực, loại hình và diện tích.
- Bản đồ tin đăng.
- Dự đoán giá bất động sản.
- Đánh giá mô hình và feature importance.
- Phân cụm thị trường.
- Bảng dữ liệu tin đăng.
- Trợ lý phân tích theo câu hỏi đơn giản.

### 16.4. Dự Đoán Trong Dashboard

Khi người dùng nhập thông tin bất động sản, dashboard tạo một dòng dữ liệu đầu vào phù hợp với schema model. Sau đó hệ thống:

1. Chọn model bán hoặc thuê theo loại giao dịch.
2. Chuẩn hóa biến phân loại theo level đã học.
3. Áp dụng target encoding cho phường/xã.
4. Dự đoán `log_price`.
5. Giới hạn dự đoán trong khoảng log giá hợp lý của tập train.
6. Chuyển ngược về giá VND bằng `expm1`.

Quy trình này đảm bảo dữ liệu nhập từ dashboard tương thích với mô hình đã train.

## 17. Công Nghệ Và Package Sử Dụng

### 17.1. Ngôn Ngữ R

R là ngôn ngữ lập trình mạnh trong thống kê, phân tích dữ liệu và trực quan hóa. Đồ án sử dụng R vì:

- Có hệ sinh thái package phong phú cho dữ liệu.
- Dễ xử lý bảng dữ liệu.
- Có Shiny để triển khai dashboard.
- Phù hợp với môn học Lập trình R.

### 17.2. Nhóm Package Thu Thập Dữ Liệu

| Package | Vai trò |
|---|---|
| `httr` | Gửi HTTP request tới API hoặc website |
| `jsonlite` | Đọc và chuyển JSON thành data frame |
| `rvest` | Trích xuất dữ liệu từ HTML |
| `xml2` | Phân tích cấu trúc XML/HTML |
| `purrr` | Lặp qua nhiều trang, nhiều nguồn |
| `furrr` | Hỗ trợ xử lý song song |
| `future` | Cấu hình worker xử lý song song |

### 17.3. Nhóm Package Xử Lý Dữ Liệu

| Package | Vai trò |
|---|---|
| `dplyr` | Lọc, biến đổi, gom nhóm dữ liệu |
| `readr` | Đọc và ghi CSV |
| `stringr` | Xử lý chuỗi và regex |
| `lubridate` | Xử lý ngày giờ |
| `tibble` | Cấu trúc bảng dữ liệu hiện đại |

### 17.4. Nhóm Package Lưu Trữ

| Package | Vai trò |
|---|---|
| `DBI` | Giao tiếp cơ sở dữ liệu |
| `RSQLite` | Lưu dữ liệu SQLite |

SQLite được dùng làm cache cho dữ liệu Chợ Tốt. CSV được dùng rộng rãi vì dễ kiểm tra, dễ chia sẻ và phù hợp với pipeline học máy.

### 17.5. Nhóm Package Mô Hình

| Package | Vai trò |
|---|---|
| `randomForest` | Huấn luyện Random Forest |
| `xgboost` | Huấn luyện XGBoost |
| `Matrix` | Tạo sparse matrix cho XGBoost |

### 17.6. Nhóm Package Trực Quan Và Ứng Dụng

| Package | Vai trò |
|---|---|
| `ggplot2` | Biểu đồ tĩnh |
| `plotly` | Biểu đồ tương tác |
| `leaflet` | Bản đồ tương tác |
| `DT` | Bảng dữ liệu tương tác |
| `shiny` | Ứng dụng web dashboard |

## 18. Kiến Trúc Hệ Thống

### 18.1. Kiến Trúc Tổng Quát

Hệ thống gồm các lớp chính:

1. Lớp thu thập dữ liệu: scraper và import CSV.
2. Lớp chuẩn hóa dữ liệu: merge sources, district normalization.
3. Lớp xử lý đặc trưng: feature engineering.
4. Lớp phân tích: EDA và biểu đồ.
5. Lớp mô hình: train, evaluate, save model.
6. Lớp giao diện: Shiny dashboard.

### 18.2. Luồng Dữ Liệu

Luồng dữ liệu tổng quát:

```text
Nguồn dữ liệu
  -> Scraper / Import
  -> Raw CSV riêng từng nguồn
  -> Combined raw CSV
  -> Featured CSV
  -> EDA plots + ML models
  -> Shiny dashboard
```

Các file quan trọng:

- `data/interim/du_lieu_gop_nguon.csv`: dữ liệu thô đã gộp.
- `data/main/du_lieu_chinh.csv`: dữ liệu sau feature engineering.
- `models/mo_hinh_gia_ban.rds`: model bán.
- `models/mo_hinh_gia_thue.rds`: model thuê.
- `models/chi_so_mo_hinh.csv`: chỉ số đánh giá.
- `models/cum_gia_quan_huyen.csv`: kết quả phân cụm.
- `plots/`: biểu đồ EDA và mô hình.

### 18.3. Pipeline Tự Động

File `scripts/pipeline/chay_pipeline.R` chạy toàn bộ quy trình gồm 11 bước:

1. Scrape dữ liệu Chợ Tốt.
2. Scrape dữ liệu Alonhadat.
3. Import dữ liệu Alonhadat bổ sung.
4. Scrape dữ liệu Lựa Chọn Nhà Đất.
5. Scrape dữ liệu Mua Bán.
6. Import dữ liệu Mogi.
7. Import dữ liệu Homedy.
8. Gộp dữ liệu nhiều nguồn.
9. Feature engineering.
10. Tạo biểu đồ EDA.
11. Train model.

Sau pipeline, người dùng có thể chạy:

```text
Rscript chay_ung_dung.R
```

để mở dashboard tại địa chỉ local.

## 19. Cấu Trúc Dữ Liệu Và File Trong Đồ Án

### 19.1. Dữ Liệu Nguồn

Các file dữ liệu thô hoặc dữ liệu nguồn:

- `data/raw/chotot/chotot_schema_chuan.csv`
- `data/raw/alonhadat/alonhadat_schema_chuan.csv`
- `data/raw/alonhadat/alonhadat_local_schema_chuan.csv`
- `data/raw/luachonnhadat/luachonnhadat_schema_chuan.csv`
- `data/raw/muaban/muaban_schema_chuan.csv`
- `data/raw/mogi/mogi_schema_chuan.csv`
- `data/raw/homedy/homedy_schema_chuan.csv`

### 19.2. Dữ Liệu Sau Gộp

File:

```text
data/interim/du_lieu_gop_nguon.csv
```

File này chứa dữ liệu đã chuẩn hóa schema từ nhiều nguồn.

### 19.3. Dữ Liệu Sau Feature Engineering

File:

```text
data/main/du_lieu_chinh.csv
```

File này được dùng cho EDA, train model và dashboard.

### 19.4. Mô Hình Và Kết Quả Train

Các file trong thư mục `models/`:

- `mo_hinh_gia_ban.rds`: bundle mô hình cho phân khúc bán.
- `mo_hinh_gia_thue.rds`: bundle mô hình cho phân khúc thuê.
- `chi_so_mo_hinh.csv`: bảng chỉ số đánh giá mô hình.
- `dang_ky_mo_hinh.csv`: model tốt nhất theo từng phân khúc.
- `do_quan_trong_bien_ban.csv`: độ quan trọng đặc trưng cho giá bán.
- `do_quan_trong_bien_thue.csv`: độ quan trọng đặc trưng cho giá thuê.
- `mo_hinh_phan_cum_gia_dien_tich.rds`: mô hình K-Means.
- `cum_gia_quan_huyen.csv`: kết quả phân cụm.

### 19.5. Biểu Đồ

Thư mục `plots/` chứa các biểu đồ:

- Phân phối log giá.
- Giá theo quận.
- Diện tích so với giá.
- Top quận theo số lượng tin.
- Top quận theo giá/m².
- Xu hướng tin đăng theo ngày.
- Phân bố địa lý.
- Giá/m² theo loại hình.
- Feature importance.
- Actual vs predicted.

## 20. Ý Nghĩa Các Biến Đặc Trưng Chính

| Biến | Ý nghĩa |
|---|---|
| `price` | Giá bất động sản |
| `area` | Diện tích |
| `rooms` | Số phòng |
| `district_name` | Quận/huyện |
| `ward` | Phường/xã |
| `category_name` | Loại bất động sản |
| `price_per_m2` | Giá trên mét vuông |
| `log_price` | Log của giá |
| `log_area` | Log của diện tích |
| `distance_to_center` | Khoảng cách tới trung tâm TP.HCM |
| `ward_price_encoded` | Mã hóa mặt bằng giá theo phường/xã |
| `posted_hour` | Giờ đăng tin |
| `posted_wday` | Thứ đăng tin |
| `listing_age_days` | Tuổi tin đăng |
| `title_has_frontage` | Tiêu đề có nhắc mặt tiền |
| `title_has_alley` | Tiêu đề có nhắc hẻm/ngõ |
| `title_has_car_access` | Có dấu hiệu ô tô/hẻm xe hơi |
| `title_has_elevator` | Có thang máy |
| `title_has_legal` | Có pháp lý/sổ hồng |
| `title_has_furnished` | Có nội thất |
| `is_rent` | Tin cho thuê hay bán |

## 21. Hạn Chế Của Dữ Liệu Và Mô Hình

### 21.1. Hạn Chế Dữ Liệu

Dữ liệu tin đăng không hoàn toàn phản ánh giá giao dịch thực tế. Giá trong tin đăng là giá chào bán hoặc giá chào thuê, có thể khác với giá giao dịch cuối cùng.

Một số hạn chế khác:

- Tin đăng có thể bị trùng hoặc đăng lại.
- Người đăng có thể nhập sai giá, diện tích hoặc vị trí.
- Một số nguồn không có tọa độ chính xác.
- Tiêu đề và mô tả không chuẩn hóa.
- Dữ liệu có thể lệch về những khu vực có nhiều tin đăng.
- Một số phân khúc ít dữ liệu dẫn đến mô hình kém ổn định.

### 21.2. Hạn Chế Mô Hình

Mô hình dự đoán dựa trên dữ liệu đã thu thập, do đó có các hạn chế:

- Không đảm bảo đúng tuyệt đối cho từng bất động sản cụ thể.
- Không xét đầy đủ yếu tố pháp lý, chất lượng xây dựng, hướng nhà, nội thất chi tiết, quy hoạch, hạ tầng tương lai.
- Không dự đoán tốt ngoài phạm vi dữ liệu đã học.
- Giá thị trường thay đổi theo thời gian, cần cập nhật dữ liệu và retrain định kỳ.

### 21.3. Cách Giảm Hạn Chế

Có thể cải thiện hệ thống bằng cách:

- Thu thập thêm dữ liệu từ nhiều nguồn.
- Tách mô hình chi tiết hơn theo loại hình.
- Thêm dữ liệu quy hoạch, giao thông, tiện ích, khoảng cách tới trường học, bệnh viện, tuyến metro.
- Dùng cross-validation thay vì chỉ một lần train/test split.
- Tối ưu tham số mô hình sâu hơn.
- Theo dõi drift dữ liệu và retrain định kỳ.

## 22. Tính Ứng Dụng Của Đồ Án

Đồ án có thể được ứng dụng trong:

- Hỗ trợ người mua/thuê tham khảo mặt bằng giá.
- Hỗ trợ người bán/cho thuê định giá sơ bộ.
- Phân tích xu hướng thị trường theo khu vực.
- So sánh giá giữa các loại hình bất động sản.
- Xây dựng dashboard phục vụ học tập, nghiên cứu hoặc demo hệ thống phân tích dữ liệu.

Kết quả dự đoán nên được xem là tham khảo, không thay thế thẩm định chuyên nghiệp hoặc quyết định đầu tư thực tế.

## 23. Lý Thuyết Xác Suất, Thống Kê Và Suy Luận Trong Đồ Án

Phần này dùng để nối trực tiếp kiến thức xác suất thống kê đã học với các chức năng trong dashboard. Nếu làm slide, có thể tách thành các nhóm: thống kê mô tả, xác suất, phân phối mẫu, bootstrap, kiểm định giả thuyết và diễn giải kết quả.

### 23.1. Tổng Thể, Mẫu Và Biến Ngẫu Nhiên

Trong thống kê, tổng thể là toàn bộ đối tượng ta muốn nghiên cứu. Với đề tài này, tổng thể lý tưởng là toàn bộ tin đăng hoặc toàn bộ bất động sản bán/cho thuê tại TP.HCM trong một giai đoạn.

Mẫu là phần dữ liệu thu thập được từ các nguồn như website, API hoặc file CSV. Dữ liệu của đồ án là mẫu quan sát từ thị trường, không phải toàn bộ thị trường.

Một biến ngẫu nhiên là đại lượng có thể nhận nhiều giá trị khác nhau tùy quan sát. Trong đồ án:

- `price`: giá bán hoặc giá thuê.
- `area`: diện tích.
- `price_per_m2`: giá trên mét vuông.
- `district_name`: khu vực.
- `category_name`: loại bất động sản.
- `source`: nguồn dữ liệu.

Nếu gọi `X` là giá/m² của một tin đăng được chọn ngẫu nhiên, thì `X` là biến ngẫu nhiên. Khi lấy dữ liệu, ta quan sát được các giá trị:

```text
x_1, x_2, ..., x_n
```

### 23.2. Thống Kê Mô Tả

Thống kê mô tả giúp tóm tắt mẫu dữ liệu trước khi suy luận hoặc huấn luyện model.

Cỡ mẫu:

```text
n = số dòng dữ liệu hợp lệ
```

Trung bình mẫu:

```text
x_bar = (1/n) * sum(x_i)
```

Trung vị:

```text
median = giá trị nằm giữa khi sắp xếp dữ liệu tăng dần
```

Nếu `n` lẻ, trung vị là giá trị thứ `(n + 1) / 2`. Nếu `n` chẵn, trung vị là trung bình của hai giá trị giữa.

Phương sai mẫu:

```text
s² = (1/(n - 1)) * sum((x_i - x_bar)^2)
```

Độ lệch chuẩn mẫu:

```text
s = sqrt(s²)
```

Sai số chuẩn của trung bình:

```text
SE = s / sqrt(n)
```

Ý nghĩa:

- `x_bar` cho biết mặt bằng trung bình.
- `median` ổn định hơn khi dữ liệu có ngoại lai.
- `s` cho biết mức phân tán.
- `SE` cho biết trung bình mẫu dao động bao nhiêu nếu lấy mẫu nhiều lần.

Trong dữ liệu bất động sản, trung vị thường đáng tin hơn trung bình vì giá có nhiều ngoại lệ. Một vài căn cực đắt có thể kéo trung bình lên rất cao, nhưng trung vị ít bị ảnh hưởng hơn.

### 23.3. Giá Trên Mét Vuông Và Ý Nghĩa Chuẩn Hóa Quy Mô

Giá tổng phụ thuộc mạnh vào diện tích. Để so sánh giữa các bất động sản khác diện tích, đồ án dùng:

```text
price_per_m2 = price / area
```

Với dữ liệu bán, đơn vị thường trình bày là triệu đồng/m²:

```text
price_per_m2_million = price_per_m2 / 1,000,000
```

Với dữ liệu thuê, có thể trình bày theo nghìn đồng/m² hoặc triệu đồng/m² tùy giao diện:

```text
price_per_m2_thousand = price_per_m2 / 1,000
```

Ý nghĩa:

- So sánh mặt bằng giá giữa các quận/huyện.
- Phát hiện khu vực đắt/rẻ theo đơn vị diện tích.
- Phục vụ phân tích xác suất, bootstrap và kiểm định giữa hai khu vực.

### 23.4. Quantile, IQR Và Ngoại Lệ

Quantile là điểm chia phân phối. Ba quantile hay dùng:

```text
Q1 = quantile 25%
Q2 = median = quantile 50%
Q3 = quantile 75%
```

Khoảng tứ phân vị:

```text
IQR = Q3 - Q1
```

Quy tắc phát hiện ngoại lệ phổ biến:

```text
lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR
```

Nếu một giá trị nhỏ hơn `lower_bound` hoặc lớn hơn `upper_bound`, nó có thể được xem là ngoại lệ. Trong bất động sản, ngoại lệ không phải lúc nào cũng sai, vì có thể là biệt thự, nhà mặt tiền trung tâm hoặc tài sản đặc biệt. Do đó, đồ án lọc giá/diện tích bất hợp lý ở mức nghiệp vụ, đồng thời vẫn dùng log transform và biểu đồ để quan sát ngoại lệ.

### 23.5. Xác Suất Thực Nghiệm

Xác suất thực nghiệm được ước lượng trực tiếp từ dữ liệu:

```text
P(A) = số quan sát thỏa sự kiện A / tổng số quan sát
```

Ví dụ:

```text
P(khu vực = Quận 1) = số tin ở Quận 1 / tổng số tin
```

```text
P(giá/m² >= Q3) = số tin có giá/m² từ Q3 trở lên / tổng số tin
```

Trong dashboard, bảng xác suất thực nghiệm tính các xác suất như:

- `P(khu vực = A)`.
- `P(loại BĐS = C)`.
- `P(giá/m² >= Q3)`.
- `P(C | A)`: xác suất loại bất động sản C khi đã biết khu vực A.
- `P(tọa độ gốc từ nguồn)`: tỷ lệ tin có tọa độ gốc thay vì tọa độ suy luận.

Các xác suất này không phải xác suất lý thuyết tuyệt đối của toàn thị trường, mà là ước lượng từ mẫu dữ liệu đã thu thập.

### 23.6. Xác Suất Có Điều Kiện

Xác suất có điều kiện đo xác suất xảy ra sự kiện A khi biết sự kiện B đã xảy ra:

```text
P(A | B) = P(A ∩ B) / P(B), với P(B) > 0
```

Trong đồ án:

```text
P(loại BĐS = căn hộ | khu vực = Quận 7)
```

được tính bằng:

```text
số tin căn hộ ở Quận 7 / tổng số tin ở Quận 7
```

Heatmap xác suất có điều kiện trong tab `Suy luận thống kê` thể hiện:

```text
P(category_name | district_name)
```

Ý nghĩa:

- Khu vực nào tập trung nhiều căn hộ, nhà phố, đất nền hoặc phòng trọ.
- Phân bố loại hình có khác nhau giữa các khu vực hay không.
- Giải thích vì sao model cần biến `district_name`, `category_name` và tương tác quận - loại hình.

### 23.7. Hàm Phân Phối Tích Lũy Thực Nghiệm ECDF

ECDF, hay empirical cumulative distribution function, mô tả xác suất một biến nhỏ hơn hoặc bằng một giá trị `x`:

```text
F_n(x) = (1/n) * sum(I(x_i <= x))
```

Trong đó `I(condition)` là hàm chỉ báo:

```text
I(condition) = 1 nếu condition đúng
I(condition) = 0 nếu condition sai
```

Trong dashboard, ECDF dùng để so sánh phân phối giá/m² giữa hai khu vực.

Ví dụ nếu tại `x = 100 triệu/m²`, ECDF của Quận A bằng 0.80, nghĩa là khoảng 80% tin ở Quận A có giá/m² không vượt quá 100 triệu/m².

Khi so sánh hai đường ECDF:

- Đường nằm bên phải thường biểu thị mặt bằng giá cao hơn.
- Đường dốc nhanh nghĩa là dữ liệu tập trung hơn.
- Đường thoải nghĩa là giá phân tán rộng hơn.

### 23.8. Kỳ Vọng, Phương Sai, Hiệp Phương Sai Và Tương Quan

Kỳ vọng là giá trị trung bình lý thuyết của biến ngẫu nhiên:

```text
E(X) = tổng x * P(X = x)          với biến rời rạc
E(X) = tích phân x*f(x) dx        với biến liên tục
```

Trong mẫu dữ liệu, kỳ vọng thường được ước lượng bằng trung bình mẫu:

```text
E(X) ≈ x_bar
```

Phương sai đo mức độ phân tán quanh kỳ vọng:

```text
Var(X) = E((X - E(X))^2)
```

Độ lệch chuẩn:

```text
SD(X) = sqrt(Var(X))
```

Hiệp phương sai giữa hai biến:

```text
Cov(X, Y) = E((X - E(X)) * (Y - E(Y)))
```

Hệ số tương quan Pearson:

```text
Corr(X, Y) = Cov(X, Y) / (SD(X) * SD(Y))
```

Trong EDA, tương quan giúp xem các biến số như diện tích, số phòng, khoảng cách tới trung tâm, giá/m² và giá tổng có quan hệ tuyến tính mạnh hay yếu. Tuy nhiên, tương quan không chứng minh quan hệ nhân quả.

### 23.9. Luật Số Lớn

Luật số lớn nói rằng khi cỡ mẫu tăng, trung bình mẫu có xu hướng tiến gần kỳ vọng thật của tổng thể:

```text
x_bar_n -> E(X) khi n -> infinity
```

Ý nghĩa trong đồ án:

- Khu vực có nhiều tin đăng thì thống kê trung bình/trung vị thường ổn định hơn.
- Khu vực có ít tin đăng thì giá trung vị hoặc xác suất ước lượng dễ dao động.
- Đây là lý do dashboard hiển thị cỡ mẫu và mô hình cần cẩn trọng với phân khúc ít dữ liệu.

### 23.10. Định Lý Giới Hạn Trung Tâm CLT

Định lý giới hạn trung tâm cho biết: nếu lấy nhiều mẫu độc lập có cùng cỡ mẫu `n`, phân phối của trung bình mẫu sẽ tiến gần phân phối chuẩn khi `n` đủ lớn, kể cả khi dữ liệu gốc không phân phối chuẩn.

Nếu tổng thể có kỳ vọng `mu` và độ lệch chuẩn `sigma`, thì:

```text
x_bar ≈ Normal(mu, sigma²/n)
```

Chuẩn hóa:

```text
Z = (x_bar - mu) / (sigma / sqrt(n)) ≈ Normal(0, 1)
```

Trong thực tế không biết `sigma`, ta dùng độ lệch chuẩn mẫu `s`:

```text
SE = s / sqrt(n)
```

Trong dashboard, biểu đồ CLT mô phỏng bằng cách:

1. Lấy nhiều mẫu có hoàn lại từ dữ liệu giá/m² đang lọc.
2. Mỗi lần lấy mẫu, tính trung bình mẫu.
3. Vẽ histogram các trung bình mẫu.
4. So sánh phân phối trung bình mẫu với mean của mẫu gốc.

Ý nghĩa khi thuyết trình:

- Dữ liệu giá/m² gốc có thể lệch, nhưng trung bình mẫu có xu hướng ổn định hơn.
- Cỡ mẫu càng lớn, phân phối trung bình mẫu càng hẹp.
- Standard error giảm theo `sqrt(n)`, nên tăng dữ liệu giúp ước lượng ổn định hơn.

### 23.11. Khoảng Tin Cậy

Khoảng tin cậy ước lượng một khoảng giá trị có khả năng chứa tham số tổng thể.

Với trung bình và giả định gần chuẩn:

```text
CI = x_bar ± z_(1-alpha/2) * SE
```

Một số giá trị `z` thường dùng:

```text
90% CI: z ≈ 1.645
95% CI: z ≈ 1.96
99% CI: z ≈ 2.576
```

Ý nghĩa của khoảng tin cậy 95%:

Nếu lặp lại quá trình lấy mẫu rất nhiều lần và mỗi lần tính một khoảng tin cậy 95%, khoảng 95% các khoảng đó sẽ chứa tham số thật. Không nên nói "xác suất tham số thật nằm trong khoảng này là 95%" theo cách tuyệt đối, vì trong thống kê tần suất tham số thật là cố định, còn khoảng tin cậy thay đổi theo mẫu.

### 23.12. Bootstrap

Bootstrap là phương pháp lấy mẫu lại có hoàn lại từ chính mẫu dữ liệu để ước lượng độ bất định của một thống kê.

Quy trình bootstrap cho trung vị giá/m²:

1. Có mẫu gốc gồm `n` giá trị:

```text
x_1, x_2, ..., x_n
```

2. Lấy ngẫu nhiên có hoàn lại `n` giá trị từ mẫu gốc để tạo mẫu bootstrap.
3. Tính trung vị của mẫu bootstrap.
4. Lặp lại `B` lần, ví dụ trong dashboard là 600 lần mặc định và có thể tăng lên.
5. Thu được phân phối bootstrap của trung vị.
6. Lấy quantile để tạo khoảng tin cậy.

Công thức percentile bootstrap:

```text
lower = quantile(bootstrap_statistics, alpha/2)
upper = quantile(bootstrap_statistics, 1 - alpha/2)
```

Với mức tin cậy 95%:

```text
alpha = 0.05
lower = quantile(boot, 0.025)
upper = quantile(boot, 0.975)
```

Lý do dùng bootstrap trong đồ án:

- Giá bất động sản lệch mạnh, không chắc phân phối chuẩn.
- Trung vị là thống kê robust nhưng công thức sai số chuẩn của trung vị không trực quan như trung bình.
- Bootstrap dễ giải thích bằng mô phỏng và phù hợp để đưa vào dashboard.

Trong dashboard, Bootstrap CI trả lời câu hỏi:

```text
Trung vị giá/m² của khu vực A có thể dao động trong khoảng nào nếu dữ liệu thu thập là một mẫu từ thị trường?
```

### 23.13. Kiểm Định Giả Thuyết

Kiểm định giả thuyết dùng để đánh giá liệu sự khác biệt quan sát được có đủ bằng chứng thống kê hay chỉ có thể do dao động mẫu.

Các thành phần chính:

- `H0`: giả thuyết không, thường là "không có khác biệt".
- `H1`: giả thuyết đối, thường là "có khác biệt".
- `alpha`: mức ý nghĩa, thường chọn 0.05.
- `p-value`: xác suất quan sát kết quả cực đoan như hiện tại hoặc hơn, nếu `H0` đúng.

Quy tắc quyết định:

```text
Nếu p-value < alpha: bác bỏ H0
Nếu p-value >= alpha: chưa đủ bằng chứng bác bỏ H0
```

Trong dashboard, kiểm định so sánh hai khu vực theo `log(giá/m²)`:

```text
H0: mean(log(price_per_m2)) của hai khu vực bằng nhau
H1: mean(log(price_per_m2)) của hai khu vực khác nhau
```

Lý do dùng log giá/m²:

- Giá/m² lệch phải mạnh.
- Log giúp giảm ảnh hưởng của giá trị cực trị.
- Khác biệt trên log có thể hiểu gần với khác biệt theo tỷ lệ.

### 23.14. t-test Hai Mẫu

t-test hai mẫu so sánh trung bình của hai nhóm. Với hai nhóm độc lập A và B:

```text
H0: mu_A = mu_B
H1: mu_A != mu_B
```

Thống kê kiểm định dạng tổng quát:

```text
t = (x_bar_A - x_bar_B) / SE_difference
```

Với Welch t-test, sai số chuẩn khác biệt:

```text
SE_difference = sqrt(s_A²/n_A + s_B²/n_B)
```

Trong đó:

- `x_bar_A`, `x_bar_B`: trung bình mẫu hai nhóm.
- `s_A²`, `s_B²`: phương sai mẫu hai nhóm.
- `n_A`, `n_B`: cỡ mẫu hai nhóm.

Đồ án dùng `t.test(log_m2 ~ group)` trong R. Mặc định `t.test()` của R dùng Welch t-test, không yêu cầu hai nhóm có phương sai bằng nhau. Điều này phù hợp với dữ liệu bất động sản vì mỗi khu vực có độ biến động khác nhau.

### 23.15. Wilcoxon Test

Wilcoxon rank-sum test là kiểm định phi tham số để so sánh hai nhóm. Kiểm định này không yêu cầu dữ liệu phân phối chuẩn như t-test.

Ý tưởng:

1. Gộp dữ liệu hai nhóm.
2. Xếp hạng tất cả giá trị.
3. So sánh tổng hạng giữa hai nhóm.

Nếu hai nhóm có phân phối giống nhau, tổng hạng không nên khác biệt quá lớn. Nếu p-value nhỏ, có bằng chứng rằng hai nhóm khác nhau.

Trong dashboard, Wilcoxon được hiển thị bên cạnh t-test như một phương án robust hơn khi dữ liệu lệch hoặc có ngoại lệ.

### 23.16. Sai Lầm Loại I Và Loại II

Khi kiểm định giả thuyết có hai loại sai lầm:

```text
Type I error: bác bỏ H0 khi H0 đúng
Type II error: không bác bỏ H0 khi H1 đúng
```

Mức ý nghĩa `alpha = 0.05` nghĩa là ta chấp nhận xác suất sai lầm loại I khoảng 5% nếu các giả định kiểm định đúng.

Trong báo cáo nên dùng câu:

```text
Chưa đủ bằng chứng bác bỏ H0
```

thay vì:

```text
Chấp nhận H0 là đúng tuyệt đối
```

Vì p-value lớn không chứng minh hai khu vực chắc chắn giống nhau, mà chỉ nói dữ liệu hiện tại chưa đủ mạnh để kết luận khác biệt.

### 23.17. Liên Hệ Lý Thuyết Với Các Tab Trong Dashboard

| Thành phần dashboard | Lý thuyết áp dụng | Ý nghĩa trình bày |
|---|---|---|
| KPI cỡ mẫu | Tổng thể, mẫu, luật số lớn | Cỡ mẫu càng lớn thì thống kê càng ổn định |
| Trung vị giá/m² | Thống kê mô tả, robust statistic | Đại diện mặt bằng giá ít bị ảnh hưởng bởi ngoại lệ |
| Standard error | Phân phối mẫu, CLT | Độ bất định của trung bình mẫu |
| P(giá cao) | Xác suất thực nghiệm | Tỷ lệ tin thuộc nhóm giá cao |
| Probability heatmap | Xác suất có điều kiện | Cấu trúc loại hình theo khu vực |
| ECDF | Hàm phân phối thực nghiệm | So sánh toàn bộ phân phối, không chỉ trung bình |
| CLT simulation | Định lý giới hạn trung tâm | Trung bình mẫu ổn định khi lấy mẫu nhiều lần |
| Bootstrap CI | Bootstrap, khoảng tin cậy | Ước lượng độ bất định của trung vị |
| Hypothesis table | t-test, Wilcoxon, p-value | Kiểm tra khác biệt giá/m² giữa hai khu vực |
| Model diagnostics | Residual, sai số, phân phối lỗi | Đánh giá mô hình ngoài các chỉ số tổng hợp |

## 24. Câu Hỏi Giám Khảo Thường Hỏi Về Model Và Trả Lời Gợi Ý

Phần này nên dùng như speaker notes khi làm PPT. Khi trả lời, nên nói ngắn gọn theo cấu trúc: mục tiêu, lý do chọn, công thức/logic, hạn chế và hướng cải thiện.

### 24.1. Vì sao chia dữ liệu 80/20?

Đồ án chia 80% train và 20% test/holdout để cân bằng giữa hai mục tiêu. Tập train cần đủ lớn để Random Forest, XGBoost và target encoding học được quy luật theo khu vực, loại hình, diện tích và nguồn dữ liệu. Tập test cũng cần đủ lớn để các chỉ số MAPE, RMSE, MAE và R² ổn định. Nếu dùng 90/10, test có thể quá ít và metric dễ dao động. Nếu dùng 70/30, train giảm nhiều, mô hình mất dữ liệu học, đặc biệt với các nhóm ít mẫu. 80/20 là lựa chọn thực tế, phổ biến và phù hợp với quy mô dữ liệu hiện tại.

### 24.2. Vì sao phải tách mô hình bán và thuê?

Giá bán và giá thuê khác bản chất:

- Giá bán thường tính bằng tỷ đồng.
- Giá thuê thường tính theo tháng.
- Phân phối, thang đo và yếu tố ảnh hưởng khác nhau.
- Một căn nhà có giá bán cao chưa chắc tỷ suất thuê tương ứng tuyến tính.

Nếu trộn chung bán và thuê vào một model, mô hình phải học hai cơ chế giá rất khác nhau, dễ nhiễu và khó giải thích. Vì vậy, đồ án tách `sale` và `rent` để mỗi mô hình học đúng phân khúc.

### 24.3. Vì sao dùng `log_price` thay vì giá gốc?

Giá bất động sản lệch phải mạnh và có nhiều ngoại lệ. Dùng `log_price = log(1 + price)` giúp giảm độ lệch, làm mô hình ổn định hơn và hạn chế việc vài bất động sản cực đắt chi phối quá trình học. Ngoài ra, sai số trên thang log gần với sai số theo tỷ lệ, phù hợp với bài toán định giá vì người dùng thường quan tâm lệch bao nhiêu phần trăm hơn là chỉ lệch bao nhiêu đồng.

### 24.4. Vì sao dùng MAPE làm tiêu chí chính?

MAPE đo sai số tương đối:

```text
MAPE = mean(abs((actual - predicted) / actual))
```

Trong bất động sản, giá có thang đo rất rộng. Lệch 1 tỷ ở căn 3 tỷ là rất lớn, nhưng lệch 1 tỷ ở căn 100 tỷ lại nhỏ hơn về tỷ lệ. MAPE giúp so sánh công bằng hơn giữa các mức giá. Tuy nhiên, đồ án vẫn xem thêm RMSE, MAE và R² để không bỏ sót lỗi lớn hoặc xu hướng tổng quát.

### 24.5. Vì sao không chỉ dùng R²?

R² đo tỷ lệ biến thiên được giải thích, nhưng không trực tiếp cho biết mô hình lệch bao nhiêu tiền hoặc bao nhiêu phần trăm. Với dữ liệu bất động sản nhiễu và có nhiều yếu tố không quan sát được, R² cần xem cùng RMSE, MAE và MAPE. Một mô hình có R² khá tốt vẫn có thể dự đoán sai nhiều ở từng tin cụ thể.

### 24.6. Vì sao chọn Linear Regression, Random Forest, XGBoost và Ensemble?

Linear Regression là baseline đơn giản, dễ giải thích. Random Forest học tốt quan hệ phi tuyến và ổn định nhờ nhiều cây độc lập. XGBoost học tuần tự để sửa lỗi, thường mạnh với dữ liệu bảng. Ensemble kết hợp Random Forest và XGBoost để tận dụng hai cơ chế khác nhau: bagging và boosting. Cách chọn này cho thấy đồ án không chỉ dùng một thuật toán mà có so sánh từ đơn giản đến mạnh hơn.

### 24.7. Random Forest khác XGBoost như thế nào?

Random Forest huấn luyện nhiều cây độc lập song song trên các mẫu bootstrap rồi lấy trung bình. Nó giảm phương sai và khá ổn định.

XGBoost huấn luyện cây tuần tự. Mỗi cây mới tập trung sửa lỗi của mô hình trước. Nó có khả năng học pattern phức tạp hơn nhưng cần kiểm soát tham số để tránh overfitting.

Tóm tắt:

```text
Random Forest = nhiều cây độc lập + lấy trung bình
XGBoost = nhiều cây tuần tự + sửa lỗi dần
```

### 24.8. Vì sao Ensemble tốt hơn?

Nếu hai mô hình sai theo các cách khác nhau, trung bình hoặc kết hợp có trọng số có thể làm dự đoán ổn định hơn. Trong đồ án:

```text
ensemble_pred = w * rf_pred + (1 - w) * xgb_pred
```

Trọng số `w` được thử từ 0 đến 1 với bước 0.05 và chọn theo MAPE thấp nhất. Kết quả hiện tại cho thấy Tuned RF/XGBoost Ensemble là model tốt nhất cho cả bán và thuê.

### 24.9. Target encoding là gì và có bị leakage không?

Target encoding mã hóa biến phân loại bằng thống kê của biến mục tiêu. Ví dụ phường/xã được mã hóa bằng mặt bằng giá trung vị có smoothing:

```text
encoded_value = log(1 + (n * median_price_group + smoothing * global_median) / (n + smoothing))
```

Để tránh leakage, encoding được fit trên train rồi áp dụng sang test. Nghĩa là giá của test không được dùng để tạo encoding cho test. Nếu gặp nhóm chưa từng xuất hiện trong train, code dùng global median làm giá trị thay thế.

### 24.10. Vì sao cần smoothing trong target encoding?

Nếu một phường chỉ có 1-2 tin, trung vị của phường đó rất dễ nhiễu. Smoothing kéo thống kê nhóm nhỏ về trung vị toàn cục:

```text
smoothed = (n * group_median + smoothing * global_median) / (n + smoothing)
```

Khi `n` lớn, nhóm tự quyết định nhiều hơn. Khi `n` nhỏ, global median có ảnh hưởng lớn hơn. Điều này giúp model không học quá mức từ nhóm ít dữ liệu.

### 24.11. Mô hình có dự đoán đúng giá giao dịch thực tế không?

Không đảm bảo. Dữ liệu là giá đăng, không phải giá giao dịch cuối cùng. Giá thực tế còn phụ thuộc thương lượng, pháp lý, tình trạng nhà, quy hoạch, hướng, nội thất, đường trước nhà và nhiều yếu tố không có trong dataset. Vì vậy, kết quả nên được xem là giá tham khảo theo mặt bằng dữ liệu thu thập.

### 24.12. Mô hình xử lý ngoại lệ như thế nào?

Đồ án xử lý ngoại lệ theo nhiều lớp:

- Lọc giá và diện tích bất hợp lý ở bước làm sạch.
- Dùng `log_price` để giảm ảnh hưởng của giá cực lớn.
- Dùng median và quantile trong phân tích vì ít nhạy với ngoại lệ.
- Giới hạn dự đoán log trong khoảng phân vị 1% - 99% của train để tránh kết quả quá cực đoan.
- Dùng biểu đồ residual và actual vs predicted để phát hiện nhóm dự đoán lệch nhiều.

### 24.13. Vì sao mô hình thuê có thể khó dự đoán hơn mô hình bán?

Giá thuê thường bị ảnh hưởng mạnh bởi nội thất, thời hạn thuê, dịch vụ đi kèm, tình trạng phòng, số người ở, điện nước, phí quản lý và yếu tố chủ nhà. Nhiều yếu tố này không có cấu trúc rõ trong dữ liệu. Vì vậy, mô hình thuê có thể có R² thấp hơn hoặc MAPE cao hơn dù quy trình huấn luyện đúng.

### 24.14. Vì sao một số khu vực model sai nhiều?

Một khu vực có thể sai nhiều vì:

- Ít dữ liệu train.
- Giá trong khu vực phân tán mạnh.
- Có nhiều loại hình trộn lẫn.
- Tin đăng thiếu thông tin quan trọng.
- Khu vực có nhiều bất động sản đặc biệt, ví dụ mặt tiền lớn, biệt thự, đất dự án.

Đó là lý do dashboard có biểu đồ MAPE theo khu vực và residual analysis để không chỉ nhìn metric tổng thể.

### 24.15. Vì sao cần kiểm định giả thuyết nếu đã có model?

Model trả lời câu hỏi dự đoán: "Giá của một bất động sản mới khoảng bao nhiêu?"

Kiểm định thống kê trả lời câu hỏi suy luận: "Hai khu vực có khác biệt giá/m² có ý nghĩa thống kê không?"

Hai phần bổ sung cho nhau. Model phục vụ dự đoán, còn kiểm định giúp giải thích thị trường và chứng minh sự khác biệt không chỉ là cảm giác nhìn biểu đồ.

### 24.16. Vì sao dùng bootstrap nếu đã có công thức khoảng tin cậy?

Công thức khoảng tin cậy cổ điển thường thuận tiện nhất cho trung bình và giả định gần chuẩn. Nhưng giá bất động sản lệch mạnh, trung vị lại phù hợp hơn trung bình. Bootstrap cho phép ước lượng khoảng tin cậy của trung vị mà không cần giả định phân phối chuẩn mạnh. Vì vậy, bootstrap rất phù hợp cho dashboard bất động sản.

### 24.17. p-value có nghĩa là gì?

p-value là xác suất quan sát kết quả cực đoan như hiện tại hoặc cực đoan hơn nếu giả thuyết H0 đúng. Nếu p-value nhỏ hơn 0.05, ta bác bỏ H0 ở mức ý nghĩa 5%. Không nên hiểu p-value là xác suất H0 đúng.

### 24.18. Nếu p-value lớn thì có kết luận hai khu vực giống nhau không?

Không. p-value lớn chỉ nói rằng dữ liệu hiện tại chưa đủ bằng chứng để bác bỏ H0. Có thể hai khu vực thật sự giống nhau, nhưng cũng có thể dữ liệu còn ít hoặc nhiễu quá lớn. Cách nói đúng là:

```text
Chưa đủ bằng chứng thống kê để kết luận hai khu vực khác nhau.
```

### 24.19. Mô hình có bị overfitting không?

Đồ án kiểm soát overfitting bằng cách đánh giá trên holdout 20%, so sánh nhiều model, dùng tham số regularization của XGBoost, dùng bagging trong Random Forest và kiểm tra actual vs predicted/residual. Tuy nhiên, để chắc hơn trong sản phẩm thật, có thể bổ sung cross-validation hoặc một tập test cuối hoàn toàn độc lập.

### 24.20. Vì sao không dùng deep learning?

Dữ liệu của đồ án chủ yếu là dữ liệu bảng có kích thước vừa phải. Với dữ liệu bảng, Random Forest và XGBoost thường hiệu quả, dễ huấn luyện và dễ giải thích hơn deep learning. Deep learning phù hợp hơn nếu có dữ liệu rất lớn, ảnh nhà, mô tả văn bản dài hoặc chuỗi thời gian phức tạp.

### 24.21. Nếu có thêm thời gian, nâng cấp model thế nào?

Các hướng nâng cấp:

- Dùng cross-validation theo thời gian hoặc theo nguồn dữ liệu.
- Tách model sâu hơn theo loại hình bất động sản.
- Thêm biến tiện ích: khoảng cách tới metro, trường học, bệnh viện, trung tâm thương mại.
- Thêm dữ liệu quy hoạch và pháp lý.
- Dùng NLP tốt hơn cho tiêu đề/mô tả.
- Thêm explainability như SHAP để giải thích từng dự đoán.
- Theo dõi drift dữ liệu và retrain định kỳ.

### 24.22. Câu trả lời ngắn khi giám khảo hỏi "Điểm mạnh nhất của model là gì?"

Điểm mạnh là pipeline đầy đủ từ làm sạch, feature engineering, tách bán/thuê, log transform, target encoding chống nhiễu, so sánh nhiều mô hình và chọn Tuned RF/XGBoost Ensemble theo MAPE. Ngoài dự đoán, đồ án còn có phần diagnostics để xem model sai ở đâu, không chỉ báo một con số tổng thể.

### 24.23. Câu trả lời ngắn khi giám khảo hỏi "Hạn chế lớn nhất là gì?"

Hạn chế lớn nhất là dữ liệu là giá đăng chứ không phải giá giao dịch thực tế, và còn thiếu nhiều yếu tố quan trọng như pháp lý chi tiết, chất lượng nhà, hướng, đường trước nhà, quy hoạch và tiện ích xung quanh. Vì vậy, kết quả dự đoán nên dùng như tham khảo định lượng, không thay thế thẩm định chuyên nghiệp.

## 25. Kết Luận Cơ Sở Lý Thuyết

Đồ án kết hợp nhiều mảng kiến thức: bất động sản, xử lý dữ liệu, khai phá dữ liệu, xác suất thống kê, suy luận thống kê, học máy, trực quan hóa và phát triển ứng dụng web bằng R. Hệ thống bắt đầu từ dữ liệu phân tán trên nhiều nguồn, sau đó chuẩn hóa, làm sạch, tạo đặc trưng và huấn luyện các mô hình hồi quy để dự đoán giá. Các mô hình Linear Regression, Random Forest, XGBoost và Ensemble được đánh giá bằng RMSE, MAE, MAPE và R². Ngoài ra, K-Means được sử dụng để phân cụm thị trường theo giá/m², diện tích và số lượng tin đăng.

Việc triển khai bằng Shiny giúp chuyển kết quả phân tích thành một dashboard tương tác, hỗ trợ người dùng xem dữ liệu, lọc tin đăng, xem bản đồ, đánh giá mô hình, thử dự đoán giá và quan sát trực tiếp các nội dung thống kê như xác suất có điều kiện, ECDF, CLT, bootstrap confidence interval và kiểm định giả thuyết. Nhờ đó, đồ án không chỉ dừng ở bước phân tích dữ liệu mà còn tạo ra một sản phẩm ứng dụng hoàn chỉnh, có thể trình bày trong báo cáo, làm slide thuyết trình và demo trực tiếp.
