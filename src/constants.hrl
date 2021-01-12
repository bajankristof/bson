%% TYPES
-define(INT32, 16).
-define(INT64, 18).
-define(LONG, 18).
-define(DOUBLE, 1).
-define(BOOLEAN, 8).
-define(STRING, 2).
-define(OBJECTID, 7).
-define(DATETIME, 9).
-define(TIMESTAMP, 17).
-define(JAVASCRIPT, 13).
-define(REGEXP, 11).
-define(BINARY, 5).
-define(DOCUMENT, 3).
-define(ARRAY, 4).
-define(NULL, 10).
-define(MIN_KEY, 255).
-define(MAX_KEY, 127).

%% SUBTYPES
-define(BINARY_SUBTYPE_DEFAULT, 0).
-define(BINARY_SUBTYPE_FUNCTION, 1).
-define(BINARY_SUBTYPE_UUID, 4).
-define(BINARY_SUBTYPE_MD5, 5).
-define(BINARY_SUBTYPE_USER_DEFINED, 5).
