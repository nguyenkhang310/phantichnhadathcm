# Cơ Sở Lý Thuyết Và Công Nghệ Ứng Dụng Trong Đồ Án Phân Tích Giá Bất Động Sản TP.HCM

## 1. Tổng Quan Đề Tài

Đồ án xây dựng hệ thống phân tích và dự đoán giá bất động sản tại Thành phố Hồ Chí Minh bằng ngôn ngữ R. Hệ thống thu thập dữ liệu tin đăng từ nhiều nguồn, chuẩn hóa dữ liệu về cùng một cấu trúc, xử lý dữ liệu, tạo đặc trưng, phân tích khám phá, huấn luyện mô hình học máy và triển khai kết quả thông qua dashboard Shiny.

Mục tiêu chính của đồ án gồm:

- Thu thập dữ liệu bất động sản từ nhiều nguồn trực tuyến như Chợ Tốt, Alonhadat, Lựa Chọn Nhà Đất, Mua Bán, Mogi và Homedy.
- Chuẩn hóa dữ liệu từ các nguồn khác nhau về cùng một schema.
- Phân tích các yếu tố ảnh hưởng đến giá bất động sản như vị trí, diện tích, loại hình, số phòng, đặc điểm tiêu đề, thời gian đăng tin và khoảng cách tới trung tâm.
- Xây dựng mô hình dự đoán giá bán và giá thuê bất động sản.
- Đánh giá mô hình bằng các chỉ số RMSE, MAE, MAPE và R bình phương.
- Trực quan hóa dữ liệu, mô hình và kết quả dự đoán bằng ứng dụng web Shiny.

Về bản chất, đồ án là một bài toán khoa học dữ liệu ứng dụng trong lĩnh vực bất động sản. Quy trình tổng quát gồm các bước:

1. Thu thập dữ liệu.
2. Làm sạch và chuẩn hóa dữ liệu.
3. Tạo đặc trưng phục vụ phân tích và mô hình hóa.
4. Phân tích khám phá dữ liệu.
5. Huấn luyện và đánh giá mô hình học máy.
6. Triển khai kết quả trên dashboard tương tác.

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

Vì giá là biến liên tục, bài toán thuộc nhóm hồi quy. Mục tiêu của hồi quy là dự đoán một giá trị số. Trong đồ án, mô hình thực tế dự đoán `log_price`, sau đó chuyển ngược về giá VND.

Các mô hình hồi quy được sử dụng:

- Linear Regression.
- Random Forest Regression.
- XGBoost Regression.
- Ensemble giữa Random Forest và XGBoost.

### 8.4. Chia Train/Test

Để đánh giá mô hình, dữ liệu được chia thành tập train và tập test. Tập train dùng để huấn luyện, tập test dùng để kiểm tra mô hình trên dữ liệu chưa thấy.

Đồ án dùng tỷ lệ 80/20:

```text
train = 80% dữ liệu
test = 20% dữ liệu
```

Việc chia dữ liệu được stratified theo nguồn dữ liệu khi có đủ mẫu, giúp tập train/test giữ được sự đa dạng giữa các nguồn.

### 8.5. Overfitting Và Underfitting

Overfitting xảy ra khi mô hình học quá sát dữ liệu train, bao gồm cả nhiễu, dẫn đến dự đoán kém trên dữ liệu mới.

Underfitting xảy ra khi mô hình quá đơn giản, không học được quy luật quan trọng trong dữ liệu.

Trong đồ án, việc đánh giá trên tập test, so sánh nhiều mô hình và dùng chỉ số sai số giúp phát hiện phần nào hai hiện tượng này.

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

### 9.2. Ưu Điểm

- Dễ hiểu và dễ giải thích.
- Huấn luyện nhanh.
- Phù hợp làm mô hình baseline.
- Có thể cho biết chiều tác động của từng biến nếu dữ liệu phù hợp.

### 9.3. Hạn Chế

- Khó học quan hệ phi tuyến phức tạp.
- Nhạy cảm với ngoại lai.
- Cần giả định tương đối tuyến tính giữa đặc trưng và biến mục tiêu.

### 9.4. Ứng Dụng Trong Đồ Án

Trong đồ án, Linear Regression được dùng như mô hình cơ sở để so sánh với các mô hình mạnh hơn. Nếu mô hình phi tuyến như Random Forest hoặc XGBoost không cải thiện nhiều, Linear Regression vẫn là lựa chọn có tính giải thích tốt.

## 10. Random Forest

### 10.1. Khái Niệm

Random Forest là thuật toán ensemble dựa trên nhiều cây quyết định. Với bài toán hồi quy, mỗi cây đưa ra một giá trị dự đoán, sau đó Random Forest lấy trung bình các dự đoán.

```text
prediction = average(prediction_tree_1, prediction_tree_2, ..., prediction_tree_n)
```

Mỗi cây được huấn luyện trên một mẫu bootstrap của dữ liệu và tại mỗi lần chia nhánh chỉ xét một tập con ngẫu nhiên các biến.

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

### 10.5. Ứng Dụng Trong Đồ Án

Trong `scripts/models/huan_luyen_mo_hinh.R`, Random Forest được train với:

```text
ntree = 300
importance = TRUE
```

Mô hình này vừa phục vụ dự đoán, vừa phục vụ phân tích tầm quan trọng của đặc trưng.

## 11. XGBoost

### 11.1. Khái Niệm

XGBoost là thuật toán gradient boosting mạnh, xây dựng nhiều cây quyết định theo cách tuần tự. Mỗi cây mới cố gắng sửa lỗi của các cây trước đó. Với bài toán hồi quy, XGBoost tối ưu hàm mất mát để giảm sai số dự đoán.

Ý tưởng tổng quát:

```text
model = tree_1 + tree_2 + ... + tree_n
```

Mỗi cây được thêm vào nhằm cải thiện phần sai số còn lại.

### 11.2. Gradient Boosting

Gradient Boosting là kỹ thuật ensemble trong đó mô hình mới được huấn luyện dựa trên gradient của hàm mất mát. Thay vì tạo nhiều mô hình độc lập như Random Forest, boosting tạo mô hình theo chuỗi, mô hình sau phụ thuộc vào sai số của mô hình trước.

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

- `nrounds`: số vòng boosting.
- `learning_rate`: tốc độ học.
- `max_depth`: độ sâu tối đa của cây.
- `min_child_weight`: điều kiện tối thiểu để chia node.
- `subsample`: tỷ lệ mẫu dùng cho mỗi vòng.
- `colsample_bytree`: tỷ lệ biến dùng cho mỗi cây.

Mô hình được chọn theo MAPE thấp nhất, nếu MAPE bằng nhau thì xét RMSE.

### 11.6. Sparse Matrix

XGBoost thường nhận dữ liệu dạng ma trận số. Với biến phân loại như quận, loại hình và thứ đăng tin, đồ án dùng `sparse.model.matrix` để tạo ma trận one-hot encoding dạng sparse. Sparse matrix giúp tiết kiệm bộ nhớ khi có nhiều biến giả bằng 0.

## 12. Ensemble Model

### 12.1. Khái Niệm Ensemble

Ensemble là phương pháp kết hợp nhiều mô hình để tạo dự đoán cuối cùng. Ý tưởng là các mô hình khác nhau có thể học các khía cạnh khác nhau của dữ liệu; khi kết hợp, kết quả có thể ổn định hơn.

### 12.2. Ensemble Trong Đồ Án

Đồ án dùng ensemble đơn giản giữa Random Forest và XGBoost:

```text
ensemble_pred = (rf_pred + xgb_pred) / 2
```

Dự đoán cuối cùng là trung bình của hai mô hình. Cách này dễ triển khai, giảm phụ thuộc vào một mô hình duy nhất và thường giúp kết quả ổn định hơn nếu hai mô hình có sai số khác nhau.

### 12.3. Chọn Mô Hình Tốt Nhất

Sau khi train, đồ án so sánh bốn lựa chọn:

- Linear Regression.
- Random Forest.
- XGBoost.
- RF + XGBoost Ensemble.

Mô hình tốt nhất được chọn theo MAPE thấp nhất, sau đó xét RMSE. Kết quả được lưu trong:

```text
models/dang_ky_mo_hinh.csv
```

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

### 13.3. Chuẩn Hóa Dữ Liệu Trước Khi Phân Cụm

Các biến như giá/m², diện tích và số lượng tin có thang đo khác nhau. Nếu không chuẩn hóa, biến có giá trị lớn sẽ chi phối khoảng cách. Vì vậy, đồ án dùng `scale()` trước khi chạy K-Means.

### 13.4. Ứng Dụng Trong Đồ Án

Dữ liệu phân cụm gồm:

- `median_price_per_m2`: giá/m² trung vị.
- `median_area`: diện tích trung vị.
- `listing_count`: số lượng tin đăng.

Kết quả được lưu vào:

- `models/mo_hinh_phan_cum_gia_dien_tich.rds`
- `models/cum_gia_quan_huyen.csv`

Phân cụm giúp nhận diện các nhóm thị trường như khu vực giá cao, khu vực diện tích lớn, khu vực có nhiều tin đăng hoặc phân khúc giá thấp hơn.

## 14. Đánh Giá Mô Hình

### 14.1. RMSE

RMSE, hay Root Mean Squared Error, đo căn bậc hai trung bình bình phương sai số:

```text
RMSE = sqrt(mean((actual - predicted)^2))
```

RMSE phạt mạnh các sai số lớn, phù hợp khi muốn chú ý đến các dự đoán lệch nhiều.

### 14.2. MAE

MAE, hay Mean Absolute Error, đo trung bình trị tuyệt đối sai số:

```text
MAE = mean(abs(actual - predicted))
```

MAE dễ hiểu vì cùng đơn vị với giá. Ví dụ MAE bằng 500 triệu nghĩa là trung bình mô hình lệch khoảng 500 triệu đồng.

### 14.3. MAPE

MAPE, hay Mean Absolute Percentage Error, đo sai số phần trăm tuyệt đối trung bình:

```text
MAPE = mean(abs((actual - predicted) / actual))
```

MAPE giúp so sánh sai số tương đối giữa các mức giá khác nhau. Ví dụ MAPE 10% nghĩa là dự đoán trung bình lệch khoảng 10% so với giá thực tế.

Trong đồ án, MAPE được dùng làm tiêu chí chính để chọn mô hình tốt nhất.

### 14.4. R Bình Phương

R bình phương đo tỷ lệ biến thiên của biến mục tiêu được mô hình giải thích:

```text
R² = 1 - SS_res / SS_tot
```

Trong đó:

- `SS_res` là tổng bình phương sai số.
- `SS_tot` là tổng bình phương độ lệch so với trung bình.

R² càng gần 1 thì mô hình giải thích dữ liệu càng tốt. Tuy nhiên, với dữ liệu bất động sản nhiều nhiễu, R² chỉ nên xem cùng với RMSE, MAE và MAPE.

### 14.5. Actual Vs Predicted

Biểu đồ actual vs predicted so sánh giá thực tế và giá dự đoán. Nếu mô hình tốt, các điểm sẽ nằm gần đường chéo `y = x`.

Đồ án xuất biểu đồ:

- `plots/du_doan_so_voi_thuc_te_ban.png`
- `plots/du_doan_so_voi_thuc_te_thue.png`

Biểu đồ này giúp phát hiện mô hình có xu hướng dự đoán thấp ở giá cao hoặc dự đoán cao ở giá thấp hay không.

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
Rscript run_app.R
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

## 23. Kết Luận Cơ Sở Lý Thuyết

Đồ án kết hợp nhiều mảng kiến thức: bất động sản, xử lý dữ liệu, khai phá dữ liệu, học máy, trực quan hóa và phát triển ứng dụng web bằng R. Hệ thống bắt đầu từ dữ liệu phân tán trên nhiều nguồn, sau đó chuẩn hóa, làm sạch, tạo đặc trưng và huấn luyện các mô hình hồi quy để dự đoán giá. Các mô hình Linear Regression, Random Forest, XGBoost và Ensemble được đánh giá bằng RMSE, MAE, MAPE và R². Ngoài ra, K-Means được sử dụng để phân cụm thị trường theo giá/m², diện tích và số lượng tin đăng.

Việc triển khai bằng Shiny giúp chuyển kết quả phân tích thành một dashboard tương tác, hỗ trợ người dùng xem dữ liệu, lọc tin đăng, xem bản đồ, đánh giá mô hình và thử dự đoán giá. Nhờ đó, đồ án không chỉ dừng ở bước phân tích dữ liệu mà còn tạo ra một sản phẩm ứng dụng hoàn chỉnh, có thể trình bày trong báo cáo và demo trực tiếp.
