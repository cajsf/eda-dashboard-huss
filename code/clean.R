# clean.R
# Gapminder 데이터 품질 확인 스크립트
# 사용법: Rscript code/clean.R   (프로젝트 루트에서 실행)
# 입력: data/gapminder.csv

# ---- 0. 설정 ----
input_path  <- "data/gapminder.csv"
expected_cols <- c("country", "year", "pop", "continent", "lifeExp", "gdpPercap")

section <- function(title) {
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat(title, "\n")
  cat(strrep("=", 60), "\n", sep = "")
}

# ---- 1. 데이터 읽기 ----
if (!file.exists(input_path)) {
  stop(sprintf("입력 파일을 찾을 수 없습니다: %s", input_path))
}
df <- read.csv(input_path, stringsAsFactors = FALSE, encoding = "UTF-8")

section("1. 기본 정보")
cat(sprintf("행 수: %d\n", nrow(df)))
cat(sprintf("열 수: %d\n", ncol(df)))
cat("열 이름:", paste(names(df), collapse = ", "), "\n")
cat("\n열별 데이터 타입:\n")
print(sapply(df, class))

# ---- 2. 스키마(열) 검증 ----
section("2. 스키마 검증")
missing_cols <- setdiff(expected_cols, names(df))
extra_cols   <- setdiff(names(df), expected_cols)
cat(if (length(missing_cols) == 0) "  OK: 필수 열 모두 존재\n"
    else sprintf("  [경고] 누락된 열: %s\n", paste(missing_cols, collapse = ", ")))
cat(if (length(extra_cols) == 0) "  OK: 예상 외 열 없음\n"
    else sprintf("  [정보] 추가 열: %s\n", paste(extra_cols, collapse = ", ")))

# ---- 3. 결측치 ----
section("3. 결측치(NA) 확인")
na_counts <- sapply(df, function(x) sum(is.na(x)))
print(na_counts)
cat(sprintf("\n총 결측치: %d\n", sum(na_counts)))
# 빈 문자열도 확인
empty_str <- sapply(df, function(x) if (is.character(x)) sum(trimws(x) == "") else 0L)
if (sum(empty_str) > 0) {
  cat("\n빈 문자열 개수:\n"); print(empty_str)
} else {
  cat("빈 문자열 없음\n")
}

# ---- 4. 중복 행 ----
section("4. 중복 확인")
dup_full <- sum(duplicated(df))
cat(sprintf("완전 중복 행: %d\n", dup_full))
if (all(c("country", "year") %in% names(df))) {
  key <- df[, c("country", "year")]
  dup_key <- sum(duplicated(key))
  cat(sprintf("(country, year) 키 중복: %d\n", dup_key))
  if (dup_key > 0) {
    cat("중복 키 예시:\n")
    print(head(key[duplicated(key) | duplicated(key, fromLast = TRUE), ]))
  }
}

# ---- 5. 값 범위 / 유효성 ----
section("5. 값 범위 및 유효성")
num_summary <- function(col) {
  if (!col %in% names(df)) return(invisible())
  x <- df[[col]]
  cat(sprintf("\n[%s]\n", col))
  print(summary(x))
}
for (c in c("year", "pop", "lifeExp", "gdpPercap")) num_summary(c)

cat("\n-- 논리적 이상치 검사 --\n")
flag <- function(cond, msg) {
  n <- sum(cond, na.rm = TRUE)
  cat(sprintf("  %s %s: %d건\n", if (n == 0) "OK " else "[경고]", msg, n))
}
if ("pop" %in% names(df))       flag(df$pop <= 0,                      "인구 <= 0")
if ("lifeExp" %in% names(df))   flag(df$lifeExp < 0 | df$lifeExp > 120, "기대수명 범위(0~120) 벗어남")
if ("gdpPercap" %in% names(df)) flag(df$gdpPercap <= 0,                "1인당 GDP <= 0")
if ("year" %in% names(df))      flag(df$year < 1900 | df$year > 2100,  "연도 범위(1900~2100) 벗어남")

# ---- 6. 범주형 값 확인 ----
section("6. 범주형 값 확인")
if ("continent" %in% names(df)) {
  cat("대륙(continent) 분포:\n")
  print(table(df$continent, useNA = "ifany"))
}
if ("country" %in% names(df)) {
  cat(sprintf("\n고유 국가 수: %d\n", length(unique(df$country))))
}
if ("year" %in% names(df)) {
  yrs <- sort(unique(df$year))
  cat(sprintf("고유 연도 수: %d  (범위 %d ~ %d)\n", length(yrs), min(yrs), max(yrs)))
  cat("연도 목록:", paste(yrs, collapse = ", "), "\n")
}

# ---- 7. 패널 완전성 (국가별 연도 수) ----
section("7. 패널 완전성 (국가별 관측치 수)")
if (all(c("country", "year") %in% names(df))) {
  per_country <- table(df$country)
  expected_years <- length(unique(df$year))
  incomplete <- per_country[per_country != expected_years]
  cat(sprintf("국가별 기대 관측치 수(=고유 연도 수): %d\n", expected_years))
  if (length(incomplete) == 0) {
    cat("  OK: 모든 국가가 동일한 연도 수를 가짐 (균형 패널)\n")
  } else {
    cat(sprintf("  [경고] 관측치 수가 다른 국가: %d개\n", length(incomplete)))
    print(incomplete)
  }
}

section("품질 확인 완료")
