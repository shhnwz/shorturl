**Problem Statement**
--------------------
Design a system to generate short url from a given input url.

**Requirement**
----------------
* Output short url should not exceed 40 characters.
* Short url must be unique.

**Proposed Solution**
------------

Let us assume INPUT URL as LONG_URL and OUTPUT URL as SHORT_URL.

Above stated requirement can be meet by 2-step approach.

    - Generate unique identifier or token from LONG_URL
    - Construct SHORT_URL using that token

**Unique Token Generation**

    - define function generate_token(LONG_URL) -> UNIQUE_SHORT_TOKEN
    - Output size, 7bytes <= length(UNIQUE_SHORT_TOKEN) <= 10bytes

Algo:

- X_128_bit <- md5(x)
- Y_48_bit <- split_from_left(X_128_bit)
- SEQ_NO_8_16_32_bit <- generate_seq(Y_48_bit)
- UNIQUE_SHORT_TOKEN_56_62_80_bit <- cat(Y_48_bit, SEQ_NO_8_16_32_bit)
- store_as_key_value(Key{UNIQUE_SHORT_TOKEN}, Value{LONG_URL})

**Construct SHORT_URL**

    - define function generate_short_url(UNIQUE_SHORT_TOKEN) -> SHORT_URL
    - output size, length(SHORT_URL) <= 40 characters.

Algo: **TODO**


