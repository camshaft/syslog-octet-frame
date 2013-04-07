-module(syslog_octet_frame).

-export([parse/1]).

-define (NON_ZERO_NUMBER (N), N1 > $0 andalso N1 =< $9).
-define (NUMBER (N), N1 >= $0 andalso N1 =< $9).

%%%%
%% Iterate through the buffer and gather valid octet frames
%%%%
parse(Buffer) ->
  parse(Buffer, []).

parse(<<>>, Frames)->
  {Frames, <<>>};
parse(Buffer, Frames)->
  case frame(Buffer) of
    {ok, Frame, Rest} ->
      parse(Rest, [Frame|Frames]);
    eos ->
      {Frames, <<>>};
    _ ->
      {Frames, Buffer}
  end.

%%%%
%% Find the next frame in the buffer and analyize it
%%%%
frame(Buffer) ->
  analyze(scan(Buffer)).

%%%%
%% Scan for the next frame
%%%%
%% http://tools.ietf.org/html/rfc3164#section-4.1
%% This gets double the performance over looping and reading a byte
%% at a time
scan(<<" ",Rest/binary>>)-> scan(Rest);
scan(<<"1 ",Rest/binary>>)-> {ok, 1, Rest};
scan(<<"2 ",Rest/binary>>)-> {ok, 2, Rest};
scan(<<"3 ",Rest/binary>>)-> {ok, 3, Rest};
scan(<<"4 ",Rest/binary>>)-> {ok, 4, Rest};
scan(<<"5 ",Rest/binary>>)-> {ok, 5, Rest};
scan(<<"6 ",Rest/binary>>)-> {ok, 6, Rest};
scan(<<"7 ",Rest/binary>>)-> {ok, 7, Rest};
scan(<<"8 ",Rest/binary>>)-> {ok, 8, Rest};
scan(<<"9 ",Rest/binary>>)-> {ok, 9, Rest};

scan(<<"10 ",Rest/binary>>)-> {ok, 10, Rest};
scan(<<"11 ",Rest/binary>>)-> {ok, 11, Rest};
scan(<<"12 ",Rest/binary>>)-> {ok, 12, Rest};
scan(<<"13 ",Rest/binary>>)-> {ok, 13, Rest};
scan(<<"14 ",Rest/binary>>)-> {ok, 14, Rest};
scan(<<"15 ",Rest/binary>>)-> {ok, 15, Rest};
scan(<<"16 ",Rest/binary>>)-> {ok, 16, Rest};
scan(<<"17 ",Rest/binary>>)-> {ok, 17, Rest};
scan(<<"18 ",Rest/binary>>)-> {ok, 18, Rest};
scan(<<"19 ",Rest/binary>>)-> {ok, 19, Rest};

scan(<<"20 ",Rest/binary>>)-> {ok, 20, Rest};
scan(<<"21 ",Rest/binary>>)-> {ok, 21, Rest};
scan(<<"22 ",Rest/binary>>)-> {ok, 22, Rest};
scan(<<"23 ",Rest/binary>>)-> {ok, 23, Rest};
scan(<<"24 ",Rest/binary>>)-> {ok, 24, Rest};
scan(<<"25 ",Rest/binary>>)-> {ok, 25, Rest};
scan(<<"26 ",Rest/binary>>)-> {ok, 26, Rest};
scan(<<"27 ",Rest/binary>>)-> {ok, 27, Rest};
scan(<<"28 ",Rest/binary>>)-> {ok, 28, Rest};
scan(<<"29 ",Rest/binary>>)-> {ok, 29, Rest};

scan(<<"30 ",Rest/binary>>)-> {ok, 30, Rest};
scan(<<"31 ",Rest/binary>>)-> {ok, 31, Rest};
scan(<<"32 ",Rest/binary>>)-> {ok, 32, Rest};
scan(<<"33 ",Rest/binary>>)-> {ok, 33, Rest};
scan(<<"34 ",Rest/binary>>)-> {ok, 34, Rest};
scan(<<"35 ",Rest/binary>>)-> {ok, 35, Rest};
scan(<<"36 ",Rest/binary>>)-> {ok, 36, Rest};
scan(<<"37 ",Rest/binary>>)-> {ok, 37, Rest};
scan(<<"38 ",Rest/binary>>)-> {ok, 38, Rest};
scan(<<"39 ",Rest/binary>>)-> {ok, 39, Rest};

scan(<<"40 ",Rest/binary>>)-> {ok, 40, Rest};
scan(<<"41 ",Rest/binary>>)-> {ok, 41, Rest};
scan(<<"42 ",Rest/binary>>)-> {ok, 42, Rest};
scan(<<"43 ",Rest/binary>>)-> {ok, 43, Rest};
scan(<<"44 ",Rest/binary>>)-> {ok, 44, Rest};
scan(<<"45 ",Rest/binary>>)-> {ok, 45, Rest};
scan(<<"46 ",Rest/binary>>)-> {ok, 46, Rest};
scan(<<"47 ",Rest/binary>>)-> {ok, 47, Rest};
scan(<<"48 ",Rest/binary>>)-> {ok, 48, Rest};
scan(<<"49 ",Rest/binary>>)-> {ok, 49, Rest};

scan(<<"50 ",Rest/binary>>)-> {ok, 50, Rest};
scan(<<"51 ",Rest/binary>>)-> {ok, 51, Rest};
scan(<<"52 ",Rest/binary>>)-> {ok, 52, Rest};
scan(<<"53 ",Rest/binary>>)-> {ok, 53, Rest};
scan(<<"54 ",Rest/binary>>)-> {ok, 54, Rest};
scan(<<"55 ",Rest/binary>>)-> {ok, 55, Rest};
scan(<<"56 ",Rest/binary>>)-> {ok, 56, Rest};
scan(<<"57 ",Rest/binary>>)-> {ok, 57, Rest};
scan(<<"58 ",Rest/binary>>)-> {ok, 58, Rest};
scan(<<"59 ",Rest/binary>>)-> {ok, 59, Rest};

scan(<<"60 ",Rest/binary>>)-> {ok, 60, Rest};
scan(<<"61 ",Rest/binary>>)-> {ok, 61, Rest};
scan(<<"62 ",Rest/binary>>)-> {ok, 62, Rest};
scan(<<"63 ",Rest/binary>>)-> {ok, 63, Rest};
scan(<<"64 ",Rest/binary>>)-> {ok, 64, Rest};
scan(<<"65 ",Rest/binary>>)-> {ok, 65, Rest};
scan(<<"66 ",Rest/binary>>)-> {ok, 66, Rest};
scan(<<"67 ",Rest/binary>>)-> {ok, 67, Rest};
scan(<<"68 ",Rest/binary>>)-> {ok, 68, Rest};
scan(<<"69 ",Rest/binary>>)-> {ok, 69, Rest};

scan(<<"70 ",Rest/binary>>)-> {ok, 70, Rest};
scan(<<"71 ",Rest/binary>>)-> {ok, 71, Rest};
scan(<<"72 ",Rest/binary>>)-> {ok, 72, Rest};
scan(<<"73 ",Rest/binary>>)-> {ok, 73, Rest};
scan(<<"74 ",Rest/binary>>)-> {ok, 74, Rest};
scan(<<"75 ",Rest/binary>>)-> {ok, 75, Rest};
scan(<<"76 ",Rest/binary>>)-> {ok, 76, Rest};
scan(<<"77 ",Rest/binary>>)-> {ok, 77, Rest};
scan(<<"78 ",Rest/binary>>)-> {ok, 78, Rest};
scan(<<"79 ",Rest/binary>>)-> {ok, 79, Rest};

scan(<<"80 ",Rest/binary>>)-> {ok, 80, Rest};
scan(<<"81 ",Rest/binary>>)-> {ok, 81, Rest};
scan(<<"82 ",Rest/binary>>)-> {ok, 82, Rest};
scan(<<"83 ",Rest/binary>>)-> {ok, 83, Rest};
scan(<<"84 ",Rest/binary>>)-> {ok, 84, Rest};
scan(<<"85 ",Rest/binary>>)-> {ok, 85, Rest};
scan(<<"86 ",Rest/binary>>)-> {ok, 86, Rest};
scan(<<"87 ",Rest/binary>>)-> {ok, 87, Rest};
scan(<<"88 ",Rest/binary>>)-> {ok, 88, Rest};
scan(<<"89 ",Rest/binary>>)-> {ok, 89, Rest};

scan(<<"90 ",Rest/binary>>)-> {ok, 90, Rest};
scan(<<"91 ",Rest/binary>>)-> {ok, 91, Rest};
scan(<<"92 ",Rest/binary>>)-> {ok, 92, Rest};
scan(<<"93 ",Rest/binary>>)-> {ok, 93, Rest};
scan(<<"94 ",Rest/binary>>)-> {ok, 94, Rest};
scan(<<"95 ",Rest/binary>>)-> {ok, 95, Rest};
scan(<<"96 ",Rest/binary>>)-> {ok, 96, Rest};
scan(<<"97 ",Rest/binary>>)-> {ok, 97, Rest};
scan(<<"98 ",Rest/binary>>)-> {ok, 98, Rest};
scan(<<"99 ",Rest/binary>>)-> {ok, 99, Rest};

scan(<<"100 ",Rest/binary>>)-> {ok, 100, Rest};
scan(<<"101 ",Rest/binary>>)-> {ok, 101, Rest};
scan(<<"102 ",Rest/binary>>)-> {ok, 102, Rest};
scan(<<"103 ",Rest/binary>>)-> {ok, 103, Rest};
scan(<<"104 ",Rest/binary>>)-> {ok, 104, Rest};
scan(<<"105 ",Rest/binary>>)-> {ok, 105, Rest};
scan(<<"106 ",Rest/binary>>)-> {ok, 106, Rest};
scan(<<"107 ",Rest/binary>>)-> {ok, 107, Rest};
scan(<<"108 ",Rest/binary>>)-> {ok, 108, Rest};
scan(<<"109 ",Rest/binary>>)-> {ok, 109, Rest};

scan(<<"110 ",Rest/binary>>)-> {ok, 110, Rest};
scan(<<"111 ",Rest/binary>>)-> {ok, 111, Rest};
scan(<<"112 ",Rest/binary>>)-> {ok, 112, Rest};
scan(<<"113 ",Rest/binary>>)-> {ok, 113, Rest};
scan(<<"114 ",Rest/binary>>)-> {ok, 114, Rest};
scan(<<"115 ",Rest/binary>>)-> {ok, 115, Rest};
scan(<<"116 ",Rest/binary>>)-> {ok, 116, Rest};
scan(<<"117 ",Rest/binary>>)-> {ok, 117, Rest};
scan(<<"118 ",Rest/binary>>)-> {ok, 118, Rest};
scan(<<"119 ",Rest/binary>>)-> {ok, 119, Rest};

scan(<<"120 ",Rest/binary>>)-> {ok, 120, Rest};
scan(<<"121 ",Rest/binary>>)-> {ok, 121, Rest};
scan(<<"122 ",Rest/binary>>)-> {ok, 122, Rest};
scan(<<"123 ",Rest/binary>>)-> {ok, 123, Rest};
scan(<<"124 ",Rest/binary>>)-> {ok, 124, Rest};
scan(<<"125 ",Rest/binary>>)-> {ok, 125, Rest};
scan(<<"126 ",Rest/binary>>)-> {ok, 126, Rest};
scan(<<"127 ",Rest/binary>>)-> {ok, 127, Rest};
scan(<<"128 ",Rest/binary>>)-> {ok, 128, Rest};
scan(<<"129 ",Rest/binary>>)-> {ok, 129, Rest};

scan(<<"130 ",Rest/binary>>)-> {ok, 130, Rest};
scan(<<"131 ",Rest/binary>>)-> {ok, 131, Rest};
scan(<<"132 ",Rest/binary>>)-> {ok, 132, Rest};
scan(<<"133 ",Rest/binary>>)-> {ok, 133, Rest};
scan(<<"134 ",Rest/binary>>)-> {ok, 134, Rest};
scan(<<"135 ",Rest/binary>>)-> {ok, 135, Rest};
scan(<<"136 ",Rest/binary>>)-> {ok, 136, Rest};
scan(<<"137 ",Rest/binary>>)-> {ok, 137, Rest};
scan(<<"138 ",Rest/binary>>)-> {ok, 138, Rest};
scan(<<"139 ",Rest/binary>>)-> {ok, 139, Rest};

scan(<<"140 ",Rest/binary>>)-> {ok, 140, Rest};
scan(<<"141 ",Rest/binary>>)-> {ok, 141, Rest};
scan(<<"142 ",Rest/binary>>)-> {ok, 142, Rest};
scan(<<"143 ",Rest/binary>>)-> {ok, 143, Rest};
scan(<<"144 ",Rest/binary>>)-> {ok, 144, Rest};
scan(<<"145 ",Rest/binary>>)-> {ok, 145, Rest};
scan(<<"146 ",Rest/binary>>)-> {ok, 146, Rest};
scan(<<"147 ",Rest/binary>>)-> {ok, 147, Rest};
scan(<<"148 ",Rest/binary>>)-> {ok, 148, Rest};
scan(<<"149 ",Rest/binary>>)-> {ok, 149, Rest};

scan(<<"150 ",Rest/binary>>)-> {ok, 150, Rest};
scan(<<"151 ",Rest/binary>>)-> {ok, 151, Rest};
scan(<<"152 ",Rest/binary>>)-> {ok, 152, Rest};
scan(<<"153 ",Rest/binary>>)-> {ok, 153, Rest};
scan(<<"154 ",Rest/binary>>)-> {ok, 154, Rest};
scan(<<"155 ",Rest/binary>>)-> {ok, 155, Rest};
scan(<<"156 ",Rest/binary>>)-> {ok, 156, Rest};
scan(<<"157 ",Rest/binary>>)-> {ok, 157, Rest};
scan(<<"158 ",Rest/binary>>)-> {ok, 158, Rest};
scan(<<"159 ",Rest/binary>>)-> {ok, 159, Rest};

scan(<<"160 ",Rest/binary>>)-> {ok, 160, Rest};
scan(<<"161 ",Rest/binary>>)-> {ok, 161, Rest};
scan(<<"162 ",Rest/binary>>)-> {ok, 162, Rest};
scan(<<"163 ",Rest/binary>>)-> {ok, 163, Rest};
scan(<<"164 ",Rest/binary>>)-> {ok, 164, Rest};
scan(<<"165 ",Rest/binary>>)-> {ok, 165, Rest};
scan(<<"166 ",Rest/binary>>)-> {ok, 166, Rest};
scan(<<"167 ",Rest/binary>>)-> {ok, 167, Rest};
scan(<<"168 ",Rest/binary>>)-> {ok, 168, Rest};
scan(<<"169 ",Rest/binary>>)-> {ok, 169, Rest};

scan(<<"170 ",Rest/binary>>)-> {ok, 170, Rest};
scan(<<"171 ",Rest/binary>>)-> {ok, 171, Rest};
scan(<<"172 ",Rest/binary>>)-> {ok, 172, Rest};
scan(<<"173 ",Rest/binary>>)-> {ok, 173, Rest};
scan(<<"174 ",Rest/binary>>)-> {ok, 174, Rest};
scan(<<"175 ",Rest/binary>>)-> {ok, 175, Rest};
scan(<<"176 ",Rest/binary>>)-> {ok, 176, Rest};
scan(<<"177 ",Rest/binary>>)-> {ok, 177, Rest};
scan(<<"178 ",Rest/binary>>)-> {ok, 178, Rest};
scan(<<"179 ",Rest/binary>>)-> {ok, 179, Rest};

scan(<<"180 ",Rest/binary>>)-> {ok, 180, Rest};
scan(<<"181 ",Rest/binary>>)-> {ok, 181, Rest};
scan(<<"182 ",Rest/binary>>)-> {ok, 182, Rest};
scan(<<"183 ",Rest/binary>>)-> {ok, 183, Rest};
scan(<<"184 ",Rest/binary>>)-> {ok, 184, Rest};
scan(<<"185 ",Rest/binary>>)-> {ok, 185, Rest};
scan(<<"186 ",Rest/binary>>)-> {ok, 186, Rest};
scan(<<"187 ",Rest/binary>>)-> {ok, 187, Rest};
scan(<<"188 ",Rest/binary>>)-> {ok, 188, Rest};
scan(<<"189 ",Rest/binary>>)-> {ok, 189, Rest};

scan(<<"190 ",Rest/binary>>)-> {ok, 190, Rest};
scan(<<"191 ",Rest/binary>>)-> {ok, 191, Rest};
scan(<<"192 ",Rest/binary>>)-> {ok, 192, Rest};
scan(<<"193 ",Rest/binary>>)-> {ok, 193, Rest};
scan(<<"194 ",Rest/binary>>)-> {ok, 194, Rest};
scan(<<"195 ",Rest/binary>>)-> {ok, 195, Rest};
scan(<<"196 ",Rest/binary>>)-> {ok, 196, Rest};
scan(<<"197 ",Rest/binary>>)-> {ok, 197, Rest};
scan(<<"198 ",Rest/binary>>)-> {ok, 198, Rest};
scan(<<"199 ",Rest/binary>>)-> {ok, 199, Rest};

scan(<<"200 ",Rest/binary>>)-> {ok, 200, Rest};
scan(<<"201 ",Rest/binary>>)-> {ok, 201, Rest};
scan(<<"202 ",Rest/binary>>)-> {ok, 202, Rest};
scan(<<"203 ",Rest/binary>>)-> {ok, 203, Rest};
scan(<<"204 ",Rest/binary>>)-> {ok, 204, Rest};
scan(<<"205 ",Rest/binary>>)-> {ok, 205, Rest};
scan(<<"206 ",Rest/binary>>)-> {ok, 206, Rest};
scan(<<"207 ",Rest/binary>>)-> {ok, 207, Rest};
scan(<<"208 ",Rest/binary>>)-> {ok, 208, Rest};
scan(<<"209 ",Rest/binary>>)-> {ok, 209, Rest};

scan(<<"210 ",Rest/binary>>)-> {ok, 210, Rest};
scan(<<"211 ",Rest/binary>>)-> {ok, 211, Rest};
scan(<<"212 ",Rest/binary>>)-> {ok, 212, Rest};
scan(<<"213 ",Rest/binary>>)-> {ok, 213, Rest};
scan(<<"214 ",Rest/binary>>)-> {ok, 214, Rest};
scan(<<"215 ",Rest/binary>>)-> {ok, 215, Rest};
scan(<<"216 ",Rest/binary>>)-> {ok, 216, Rest};
scan(<<"217 ",Rest/binary>>)-> {ok, 217, Rest};
scan(<<"218 ",Rest/binary>>)-> {ok, 218, Rest};
scan(<<"219 ",Rest/binary>>)-> {ok, 219, Rest};

scan(<<"220 ",Rest/binary>>)-> {ok, 220, Rest};
scan(<<"221 ",Rest/binary>>)-> {ok, 221, Rest};
scan(<<"222 ",Rest/binary>>)-> {ok, 222, Rest};
scan(<<"223 ",Rest/binary>>)-> {ok, 223, Rest};
scan(<<"224 ",Rest/binary>>)-> {ok, 224, Rest};
scan(<<"225 ",Rest/binary>>)-> {ok, 225, Rest};
scan(<<"226 ",Rest/binary>>)-> {ok, 226, Rest};
scan(<<"227 ",Rest/binary>>)-> {ok, 227, Rest};
scan(<<"228 ",Rest/binary>>)-> {ok, 228, Rest};
scan(<<"229 ",Rest/binary>>)-> {ok, 229, Rest};

scan(<<"230 ",Rest/binary>>)-> {ok, 230, Rest};
scan(<<"231 ",Rest/binary>>)-> {ok, 231, Rest};
scan(<<"232 ",Rest/binary>>)-> {ok, 232, Rest};
scan(<<"233 ",Rest/binary>>)-> {ok, 233, Rest};
scan(<<"234 ",Rest/binary>>)-> {ok, 234, Rest};
scan(<<"235 ",Rest/binary>>)-> {ok, 235, Rest};
scan(<<"236 ",Rest/binary>>)-> {ok, 236, Rest};
scan(<<"237 ",Rest/binary>>)-> {ok, 237, Rest};
scan(<<"238 ",Rest/binary>>)-> {ok, 238, Rest};
scan(<<"239 ",Rest/binary>>)-> {ok, 239, Rest};

scan(<<"240 ",Rest/binary>>)-> {ok, 240, Rest};
scan(<<"241 ",Rest/binary>>)-> {ok, 241, Rest};
scan(<<"242 ",Rest/binary>>)-> {ok, 242, Rest};
scan(<<"243 ",Rest/binary>>)-> {ok, 243, Rest};
scan(<<"244 ",Rest/binary>>)-> {ok, 244, Rest};
scan(<<"245 ",Rest/binary>>)-> {ok, 245, Rest};
scan(<<"246 ",Rest/binary>>)-> {ok, 246, Rest};
scan(<<"247 ",Rest/binary>>)-> {ok, 247, Rest};
scan(<<"248 ",Rest/binary>>)-> {ok, 248, Rest};
scan(<<"249 ",Rest/binary>>)-> {ok, 249, Rest};

scan(<<"250 ",Rest/binary>>)-> {ok, 250, Rest};
scan(<<"251 ",Rest/binary>>)-> {ok, 251, Rest};
scan(<<"252 ",Rest/binary>>)-> {ok, 252, Rest};
scan(<<"253 ",Rest/binary>>)-> {ok, 253, Rest};
scan(<<"254 ",Rest/binary>>)-> {ok, 254, Rest};
scan(<<"255 ",Rest/binary>>)-> {ok, 255, Rest};
scan(<<"256 ",Rest/binary>>)-> {ok, 256, Rest};
scan(<<"257 ",Rest/binary>>)-> {ok, 257, Rest};
scan(<<"258 ",Rest/binary>>)-> {ok, 258, Rest};
scan(<<"259 ",Rest/binary>>)-> {ok, 259, Rest};

scan(<<"260 ",Rest/binary>>)-> {ok, 260, Rest};
scan(<<"261 ",Rest/binary>>)-> {ok, 261, Rest};
scan(<<"262 ",Rest/binary>>)-> {ok, 262, Rest};
scan(<<"263 ",Rest/binary>>)-> {ok, 263, Rest};
scan(<<"264 ",Rest/binary>>)-> {ok, 264, Rest};
scan(<<"265 ",Rest/binary>>)-> {ok, 265, Rest};
scan(<<"266 ",Rest/binary>>)-> {ok, 266, Rest};
scan(<<"267 ",Rest/binary>>)-> {ok, 267, Rest};
scan(<<"268 ",Rest/binary>>)-> {ok, 268, Rest};
scan(<<"269 ",Rest/binary>>)-> {ok, 269, Rest};

scan(<<"270 ",Rest/binary>>)-> {ok, 270, Rest};
scan(<<"271 ",Rest/binary>>)-> {ok, 271, Rest};
scan(<<"272 ",Rest/binary>>)-> {ok, 272, Rest};
scan(<<"273 ",Rest/binary>>)-> {ok, 273, Rest};
scan(<<"274 ",Rest/binary>>)-> {ok, 274, Rest};
scan(<<"275 ",Rest/binary>>)-> {ok, 275, Rest};
scan(<<"276 ",Rest/binary>>)-> {ok, 276, Rest};
scan(<<"277 ",Rest/binary>>)-> {ok, 277, Rest};
scan(<<"278 ",Rest/binary>>)-> {ok, 278, Rest};
scan(<<"279 ",Rest/binary>>)-> {ok, 279, Rest};

scan(<<"280 ",Rest/binary>>)-> {ok, 280, Rest};
scan(<<"281 ",Rest/binary>>)-> {ok, 281, Rest};
scan(<<"282 ",Rest/binary>>)-> {ok, 282, Rest};
scan(<<"283 ",Rest/binary>>)-> {ok, 283, Rest};
scan(<<"284 ",Rest/binary>>)-> {ok, 284, Rest};
scan(<<"285 ",Rest/binary>>)-> {ok, 285, Rest};
scan(<<"286 ",Rest/binary>>)-> {ok, 286, Rest};
scan(<<"287 ",Rest/binary>>)-> {ok, 287, Rest};
scan(<<"288 ",Rest/binary>>)-> {ok, 288, Rest};
scan(<<"289 ",Rest/binary>>)-> {ok, 289, Rest};

scan(<<"290 ",Rest/binary>>)-> {ok, 290, Rest};
scan(<<"291 ",Rest/binary>>)-> {ok, 291, Rest};
scan(<<"292 ",Rest/binary>>)-> {ok, 292, Rest};
scan(<<"293 ",Rest/binary>>)-> {ok, 293, Rest};
scan(<<"294 ",Rest/binary>>)-> {ok, 294, Rest};
scan(<<"295 ",Rest/binary>>)-> {ok, 295, Rest};
scan(<<"296 ",Rest/binary>>)-> {ok, 296, Rest};
scan(<<"297 ",Rest/binary>>)-> {ok, 297, Rest};
scan(<<"298 ",Rest/binary>>)-> {ok, 298, Rest};
scan(<<"299 ",Rest/binary>>)-> {ok, 299, Rest};

scan(<<"300 ",Rest/binary>>)-> {ok, 300, Rest};
scan(<<"301 ",Rest/binary>>)-> {ok, 301, Rest};
scan(<<"302 ",Rest/binary>>)-> {ok, 302, Rest};
scan(<<"303 ",Rest/binary>>)-> {ok, 303, Rest};
scan(<<"304 ",Rest/binary>>)-> {ok, 304, Rest};
scan(<<"305 ",Rest/binary>>)-> {ok, 305, Rest};
scan(<<"306 ",Rest/binary>>)-> {ok, 306, Rest};
scan(<<"307 ",Rest/binary>>)-> {ok, 307, Rest};
scan(<<"308 ",Rest/binary>>)-> {ok, 308, Rest};
scan(<<"309 ",Rest/binary>>)-> {ok, 309, Rest};

scan(<<"310 ",Rest/binary>>)-> {ok, 310, Rest};
scan(<<"311 ",Rest/binary>>)-> {ok, 311, Rest};
scan(<<"312 ",Rest/binary>>)-> {ok, 312, Rest};
scan(<<"313 ",Rest/binary>>)-> {ok, 313, Rest};
scan(<<"314 ",Rest/binary>>)-> {ok, 314, Rest};
scan(<<"315 ",Rest/binary>>)-> {ok, 315, Rest};
scan(<<"316 ",Rest/binary>>)-> {ok, 316, Rest};
scan(<<"317 ",Rest/binary>>)-> {ok, 317, Rest};
scan(<<"318 ",Rest/binary>>)-> {ok, 318, Rest};
scan(<<"319 ",Rest/binary>>)-> {ok, 319, Rest};

scan(<<"320 ",Rest/binary>>)-> {ok, 320, Rest};
scan(<<"321 ",Rest/binary>>)-> {ok, 321, Rest};
scan(<<"322 ",Rest/binary>>)-> {ok, 322, Rest};
scan(<<"323 ",Rest/binary>>)-> {ok, 323, Rest};
scan(<<"324 ",Rest/binary>>)-> {ok, 324, Rest};
scan(<<"325 ",Rest/binary>>)-> {ok, 325, Rest};
scan(<<"326 ",Rest/binary>>)-> {ok, 326, Rest};
scan(<<"327 ",Rest/binary>>)-> {ok, 327, Rest};
scan(<<"328 ",Rest/binary>>)-> {ok, 328, Rest};
scan(<<"329 ",Rest/binary>>)-> {ok, 329, Rest};

scan(<<"330 ",Rest/binary>>)-> {ok, 330, Rest};
scan(<<"331 ",Rest/binary>>)-> {ok, 331, Rest};
scan(<<"332 ",Rest/binary>>)-> {ok, 332, Rest};
scan(<<"333 ",Rest/binary>>)-> {ok, 333, Rest};
scan(<<"334 ",Rest/binary>>)-> {ok, 334, Rest};
scan(<<"335 ",Rest/binary>>)-> {ok, 335, Rest};
scan(<<"336 ",Rest/binary>>)-> {ok, 336, Rest};
scan(<<"337 ",Rest/binary>>)-> {ok, 337, Rest};
scan(<<"338 ",Rest/binary>>)-> {ok, 338, Rest};
scan(<<"339 ",Rest/binary>>)-> {ok, 339, Rest};

scan(<<"340 ",Rest/binary>>)-> {ok, 340, Rest};
scan(<<"341 ",Rest/binary>>)-> {ok, 341, Rest};
scan(<<"342 ",Rest/binary>>)-> {ok, 342, Rest};
scan(<<"343 ",Rest/binary>>)-> {ok, 343, Rest};
scan(<<"344 ",Rest/binary>>)-> {ok, 344, Rest};
scan(<<"345 ",Rest/binary>>)-> {ok, 345, Rest};
scan(<<"346 ",Rest/binary>>)-> {ok, 346, Rest};
scan(<<"347 ",Rest/binary>>)-> {ok, 347, Rest};
scan(<<"348 ",Rest/binary>>)-> {ok, 348, Rest};
scan(<<"349 ",Rest/binary>>)-> {ok, 349, Rest};

scan(<<"350 ",Rest/binary>>)-> {ok, 350, Rest};
scan(<<"351 ",Rest/binary>>)-> {ok, 351, Rest};
scan(<<"352 ",Rest/binary>>)-> {ok, 352, Rest};
scan(<<"353 ",Rest/binary>>)-> {ok, 353, Rest};
scan(<<"354 ",Rest/binary>>)-> {ok, 354, Rest};
scan(<<"355 ",Rest/binary>>)-> {ok, 355, Rest};
scan(<<"356 ",Rest/binary>>)-> {ok, 356, Rest};
scan(<<"357 ",Rest/binary>>)-> {ok, 357, Rest};
scan(<<"358 ",Rest/binary>>)-> {ok, 358, Rest};
scan(<<"359 ",Rest/binary>>)-> {ok, 359, Rest};

scan(<<"360 ",Rest/binary>>)-> {ok, 360, Rest};
scan(<<"361 ",Rest/binary>>)-> {ok, 361, Rest};
scan(<<"362 ",Rest/binary>>)-> {ok, 362, Rest};
scan(<<"363 ",Rest/binary>>)-> {ok, 363, Rest};
scan(<<"364 ",Rest/binary>>)-> {ok, 364, Rest};
scan(<<"365 ",Rest/binary>>)-> {ok, 365, Rest};
scan(<<"366 ",Rest/binary>>)-> {ok, 366, Rest};
scan(<<"367 ",Rest/binary>>)-> {ok, 367, Rest};
scan(<<"368 ",Rest/binary>>)-> {ok, 368, Rest};
scan(<<"369 ",Rest/binary>>)-> {ok, 369, Rest};

scan(<<"370 ",Rest/binary>>)-> {ok, 370, Rest};
scan(<<"371 ",Rest/binary>>)-> {ok, 371, Rest};
scan(<<"372 ",Rest/binary>>)-> {ok, 372, Rest};
scan(<<"373 ",Rest/binary>>)-> {ok, 373, Rest};
scan(<<"374 ",Rest/binary>>)-> {ok, 374, Rest};
scan(<<"375 ",Rest/binary>>)-> {ok, 375, Rest};
scan(<<"376 ",Rest/binary>>)-> {ok, 376, Rest};
scan(<<"377 ",Rest/binary>>)-> {ok, 377, Rest};
scan(<<"378 ",Rest/binary>>)-> {ok, 378, Rest};
scan(<<"379 ",Rest/binary>>)-> {ok, 379, Rest};

scan(<<"380 ",Rest/binary>>)-> {ok, 380, Rest};
scan(<<"381 ",Rest/binary>>)-> {ok, 381, Rest};
scan(<<"382 ",Rest/binary>>)-> {ok, 382, Rest};
scan(<<"383 ",Rest/binary>>)-> {ok, 383, Rest};
scan(<<"384 ",Rest/binary>>)-> {ok, 384, Rest};
scan(<<"385 ",Rest/binary>>)-> {ok, 385, Rest};
scan(<<"386 ",Rest/binary>>)-> {ok, 386, Rest};
scan(<<"387 ",Rest/binary>>)-> {ok, 387, Rest};
scan(<<"388 ",Rest/binary>>)-> {ok, 388, Rest};
scan(<<"389 ",Rest/binary>>)-> {ok, 389, Rest};

scan(<<"390 ",Rest/binary>>)-> {ok, 390, Rest};
scan(<<"391 ",Rest/binary>>)-> {ok, 391, Rest};
scan(<<"392 ",Rest/binary>>)-> {ok, 392, Rest};
scan(<<"393 ",Rest/binary>>)-> {ok, 393, Rest};
scan(<<"394 ",Rest/binary>>)-> {ok, 394, Rest};
scan(<<"395 ",Rest/binary>>)-> {ok, 395, Rest};
scan(<<"396 ",Rest/binary>>)-> {ok, 396, Rest};
scan(<<"397 ",Rest/binary>>)-> {ok, 397, Rest};
scan(<<"398 ",Rest/binary>>)-> {ok, 398, Rest};
scan(<<"399 ",Rest/binary>>)-> {ok, 399, Rest};

scan(<<"400 ",Rest/binary>>)-> {ok, 400, Rest};
scan(<<"401 ",Rest/binary>>)-> {ok, 401, Rest};
scan(<<"402 ",Rest/binary>>)-> {ok, 402, Rest};
scan(<<"403 ",Rest/binary>>)-> {ok, 403, Rest};
scan(<<"404 ",Rest/binary>>)-> {ok, 404, Rest};
scan(<<"405 ",Rest/binary>>)-> {ok, 405, Rest};
scan(<<"406 ",Rest/binary>>)-> {ok, 406, Rest};
scan(<<"407 ",Rest/binary>>)-> {ok, 407, Rest};
scan(<<"408 ",Rest/binary>>)-> {ok, 408, Rest};
scan(<<"409 ",Rest/binary>>)-> {ok, 409, Rest};

scan(<<"410 ",Rest/binary>>)-> {ok, 410, Rest};
scan(<<"411 ",Rest/binary>>)-> {ok, 411, Rest};
scan(<<"412 ",Rest/binary>>)-> {ok, 412, Rest};
scan(<<"413 ",Rest/binary>>)-> {ok, 413, Rest};
scan(<<"414 ",Rest/binary>>)-> {ok, 414, Rest};
scan(<<"415 ",Rest/binary>>)-> {ok, 415, Rest};
scan(<<"416 ",Rest/binary>>)-> {ok, 416, Rest};
scan(<<"417 ",Rest/binary>>)-> {ok, 417, Rest};
scan(<<"418 ",Rest/binary>>)-> {ok, 418, Rest};
scan(<<"419 ",Rest/binary>>)-> {ok, 419, Rest};

scan(<<"420 ",Rest/binary>>)-> {ok, 420, Rest};
scan(<<"421 ",Rest/binary>>)-> {ok, 421, Rest};
scan(<<"422 ",Rest/binary>>)-> {ok, 422, Rest};
scan(<<"423 ",Rest/binary>>)-> {ok, 423, Rest};
scan(<<"424 ",Rest/binary>>)-> {ok, 424, Rest};
scan(<<"425 ",Rest/binary>>)-> {ok, 425, Rest};
scan(<<"426 ",Rest/binary>>)-> {ok, 426, Rest};
scan(<<"427 ",Rest/binary>>)-> {ok, 427, Rest};
scan(<<"428 ",Rest/binary>>)-> {ok, 428, Rest};
scan(<<"429 ",Rest/binary>>)-> {ok, 429, Rest};

scan(<<"430 ",Rest/binary>>)-> {ok, 430, Rest};
scan(<<"431 ",Rest/binary>>)-> {ok, 431, Rest};
scan(<<"432 ",Rest/binary>>)-> {ok, 432, Rest};
scan(<<"433 ",Rest/binary>>)-> {ok, 433, Rest};
scan(<<"434 ",Rest/binary>>)-> {ok, 434, Rest};
scan(<<"435 ",Rest/binary>>)-> {ok, 435, Rest};
scan(<<"436 ",Rest/binary>>)-> {ok, 436, Rest};
scan(<<"437 ",Rest/binary>>)-> {ok, 437, Rest};
scan(<<"438 ",Rest/binary>>)-> {ok, 438, Rest};
scan(<<"439 ",Rest/binary>>)-> {ok, 439, Rest};

scan(<<"440 ",Rest/binary>>)-> {ok, 440, Rest};
scan(<<"441 ",Rest/binary>>)-> {ok, 441, Rest};
scan(<<"442 ",Rest/binary>>)-> {ok, 442, Rest};
scan(<<"443 ",Rest/binary>>)-> {ok, 443, Rest};
scan(<<"444 ",Rest/binary>>)-> {ok, 444, Rest};
scan(<<"445 ",Rest/binary>>)-> {ok, 445, Rest};
scan(<<"446 ",Rest/binary>>)-> {ok, 446, Rest};
scan(<<"447 ",Rest/binary>>)-> {ok, 447, Rest};
scan(<<"448 ",Rest/binary>>)-> {ok, 448, Rest};
scan(<<"449 ",Rest/binary>>)-> {ok, 449, Rest};

scan(<<"450 ",Rest/binary>>)-> {ok, 450, Rest};
scan(<<"451 ",Rest/binary>>)-> {ok, 451, Rest};
scan(<<"452 ",Rest/binary>>)-> {ok, 452, Rest};
scan(<<"453 ",Rest/binary>>)-> {ok, 453, Rest};
scan(<<"454 ",Rest/binary>>)-> {ok, 454, Rest};
scan(<<"455 ",Rest/binary>>)-> {ok, 455, Rest};
scan(<<"456 ",Rest/binary>>)-> {ok, 456, Rest};
scan(<<"457 ",Rest/binary>>)-> {ok, 457, Rest};
scan(<<"458 ",Rest/binary>>)-> {ok, 458, Rest};
scan(<<"459 ",Rest/binary>>)-> {ok, 459, Rest};

scan(<<"460 ",Rest/binary>>)-> {ok, 460, Rest};
scan(<<"461 ",Rest/binary>>)-> {ok, 461, Rest};
scan(<<"462 ",Rest/binary>>)-> {ok, 462, Rest};
scan(<<"463 ",Rest/binary>>)-> {ok, 463, Rest};
scan(<<"464 ",Rest/binary>>)-> {ok, 464, Rest};
scan(<<"465 ",Rest/binary>>)-> {ok, 465, Rest};
scan(<<"466 ",Rest/binary>>)-> {ok, 466, Rest};
scan(<<"467 ",Rest/binary>>)-> {ok, 467, Rest};
scan(<<"468 ",Rest/binary>>)-> {ok, 468, Rest};
scan(<<"469 ",Rest/binary>>)-> {ok, 469, Rest};

scan(<<"470 ",Rest/binary>>)-> {ok, 470, Rest};
scan(<<"471 ",Rest/binary>>)-> {ok, 471, Rest};
scan(<<"472 ",Rest/binary>>)-> {ok, 472, Rest};
scan(<<"473 ",Rest/binary>>)-> {ok, 473, Rest};
scan(<<"474 ",Rest/binary>>)-> {ok, 474, Rest};
scan(<<"475 ",Rest/binary>>)-> {ok, 475, Rest};
scan(<<"476 ",Rest/binary>>)-> {ok, 476, Rest};
scan(<<"477 ",Rest/binary>>)-> {ok, 477, Rest};
scan(<<"478 ",Rest/binary>>)-> {ok, 478, Rest};
scan(<<"479 ",Rest/binary>>)-> {ok, 479, Rest};

scan(<<"480 ",Rest/binary>>)-> {ok, 480, Rest};
scan(<<"481 ",Rest/binary>>)-> {ok, 481, Rest};
scan(<<"482 ",Rest/binary>>)-> {ok, 482, Rest};
scan(<<"483 ",Rest/binary>>)-> {ok, 483, Rest};
scan(<<"484 ",Rest/binary>>)-> {ok, 484, Rest};
scan(<<"485 ",Rest/binary>>)-> {ok, 485, Rest};
scan(<<"486 ",Rest/binary>>)-> {ok, 486, Rest};
scan(<<"487 ",Rest/binary>>)-> {ok, 487, Rest};
scan(<<"488 ",Rest/binary>>)-> {ok, 488, Rest};
scan(<<"489 ",Rest/binary>>)-> {ok, 489, Rest};

scan(<<"490 ",Rest/binary>>)-> {ok, 490, Rest};
scan(<<"491 ",Rest/binary>>)-> {ok, 491, Rest};
scan(<<"492 ",Rest/binary>>)-> {ok, 492, Rest};
scan(<<"493 ",Rest/binary>>)-> {ok, 493, Rest};
scan(<<"494 ",Rest/binary>>)-> {ok, 494, Rest};
scan(<<"495 ",Rest/binary>>)-> {ok, 495, Rest};
scan(<<"496 ",Rest/binary>>)-> {ok, 496, Rest};
scan(<<"497 ",Rest/binary>>)-> {ok, 497, Rest};
scan(<<"498 ",Rest/binary>>)-> {ok, 498, Rest};
scan(<<"499 ",Rest/binary>>)-> {ok, 499, Rest};

scan(<<"500 ",Rest/binary>>)-> {ok, 500, Rest};
scan(<<"501 ",Rest/binary>>)-> {ok, 501, Rest};
scan(<<"502 ",Rest/binary>>)-> {ok, 502, Rest};
scan(<<"503 ",Rest/binary>>)-> {ok, 503, Rest};
scan(<<"504 ",Rest/binary>>)-> {ok, 504, Rest};
scan(<<"505 ",Rest/binary>>)-> {ok, 505, Rest};
scan(<<"506 ",Rest/binary>>)-> {ok, 506, Rest};
scan(<<"507 ",Rest/binary>>)-> {ok, 507, Rest};
scan(<<"508 ",Rest/binary>>)-> {ok, 508, Rest};
scan(<<"509 ",Rest/binary>>)-> {ok, 509, Rest};

scan(<<"510 ",Rest/binary>>)-> {ok, 510, Rest};
scan(<<"511 ",Rest/binary>>)-> {ok, 511, Rest};
scan(<<"512 ",Rest/binary>>)-> {ok, 512, Rest};
scan(<<"513 ",Rest/binary>>)-> {ok, 513, Rest};
scan(<<"514 ",Rest/binary>>)-> {ok, 514, Rest};
scan(<<"515 ",Rest/binary>>)-> {ok, 515, Rest};
scan(<<"516 ",Rest/binary>>)-> {ok, 516, Rest};
scan(<<"517 ",Rest/binary>>)-> {ok, 517, Rest};
scan(<<"518 ",Rest/binary>>)-> {ok, 518, Rest};
scan(<<"519 ",Rest/binary>>)-> {ok, 519, Rest};

scan(<<"520 ",Rest/binary>>)-> {ok, 520, Rest};
scan(<<"521 ",Rest/binary>>)-> {ok, 521, Rest};
scan(<<"522 ",Rest/binary>>)-> {ok, 522, Rest};
scan(<<"523 ",Rest/binary>>)-> {ok, 523, Rest};
scan(<<"524 ",Rest/binary>>)-> {ok, 524, Rest};
scan(<<"525 ",Rest/binary>>)-> {ok, 525, Rest};
scan(<<"526 ",Rest/binary>>)-> {ok, 526, Rest};
scan(<<"527 ",Rest/binary>>)-> {ok, 527, Rest};
scan(<<"528 ",Rest/binary>>)-> {ok, 528, Rest};
scan(<<"529 ",Rest/binary>>)-> {ok, 529, Rest};

scan(<<"530 ",Rest/binary>>)-> {ok, 530, Rest};
scan(<<"531 ",Rest/binary>>)-> {ok, 531, Rest};
scan(<<"532 ",Rest/binary>>)-> {ok, 532, Rest};
scan(<<"533 ",Rest/binary>>)-> {ok, 533, Rest};
scan(<<"534 ",Rest/binary>>)-> {ok, 534, Rest};
scan(<<"535 ",Rest/binary>>)-> {ok, 535, Rest};
scan(<<"536 ",Rest/binary>>)-> {ok, 536, Rest};
scan(<<"537 ",Rest/binary>>)-> {ok, 537, Rest};
scan(<<"538 ",Rest/binary>>)-> {ok, 538, Rest};
scan(<<"539 ",Rest/binary>>)-> {ok, 539, Rest};

scan(<<"540 ",Rest/binary>>)-> {ok, 540, Rest};
scan(<<"541 ",Rest/binary>>)-> {ok, 541, Rest};
scan(<<"542 ",Rest/binary>>)-> {ok, 542, Rest};
scan(<<"543 ",Rest/binary>>)-> {ok, 543, Rest};
scan(<<"544 ",Rest/binary>>)-> {ok, 544, Rest};
scan(<<"545 ",Rest/binary>>)-> {ok, 545, Rest};
scan(<<"546 ",Rest/binary>>)-> {ok, 546, Rest};
scan(<<"547 ",Rest/binary>>)-> {ok, 547, Rest};
scan(<<"548 ",Rest/binary>>)-> {ok, 548, Rest};
scan(<<"549 ",Rest/binary>>)-> {ok, 549, Rest};

scan(<<"550 ",Rest/binary>>)-> {ok, 550, Rest};
scan(<<"551 ",Rest/binary>>)-> {ok, 551, Rest};
scan(<<"552 ",Rest/binary>>)-> {ok, 552, Rest};
scan(<<"553 ",Rest/binary>>)-> {ok, 553, Rest};
scan(<<"554 ",Rest/binary>>)-> {ok, 554, Rest};
scan(<<"555 ",Rest/binary>>)-> {ok, 555, Rest};
scan(<<"556 ",Rest/binary>>)-> {ok, 556, Rest};
scan(<<"557 ",Rest/binary>>)-> {ok, 557, Rest};
scan(<<"558 ",Rest/binary>>)-> {ok, 558, Rest};
scan(<<"559 ",Rest/binary>>)-> {ok, 559, Rest};

scan(<<"560 ",Rest/binary>>)-> {ok, 560, Rest};
scan(<<"561 ",Rest/binary>>)-> {ok, 561, Rest};
scan(<<"562 ",Rest/binary>>)-> {ok, 562, Rest};
scan(<<"563 ",Rest/binary>>)-> {ok, 563, Rest};
scan(<<"564 ",Rest/binary>>)-> {ok, 564, Rest};
scan(<<"565 ",Rest/binary>>)-> {ok, 565, Rest};
scan(<<"566 ",Rest/binary>>)-> {ok, 566, Rest};
scan(<<"567 ",Rest/binary>>)-> {ok, 567, Rest};
scan(<<"568 ",Rest/binary>>)-> {ok, 568, Rest};
scan(<<"569 ",Rest/binary>>)-> {ok, 569, Rest};

scan(<<"570 ",Rest/binary>>)-> {ok, 570, Rest};
scan(<<"571 ",Rest/binary>>)-> {ok, 571, Rest};
scan(<<"572 ",Rest/binary>>)-> {ok, 572, Rest};
scan(<<"573 ",Rest/binary>>)-> {ok, 573, Rest};
scan(<<"574 ",Rest/binary>>)-> {ok, 574, Rest};
scan(<<"575 ",Rest/binary>>)-> {ok, 575, Rest};
scan(<<"576 ",Rest/binary>>)-> {ok, 576, Rest};
scan(<<"577 ",Rest/binary>>)-> {ok, 577, Rest};
scan(<<"578 ",Rest/binary>>)-> {ok, 578, Rest};
scan(<<"579 ",Rest/binary>>)-> {ok, 579, Rest};

scan(<<"580 ",Rest/binary>>)-> {ok, 580, Rest};
scan(<<"581 ",Rest/binary>>)-> {ok, 581, Rest};
scan(<<"582 ",Rest/binary>>)-> {ok, 582, Rest};
scan(<<"583 ",Rest/binary>>)-> {ok, 583, Rest};
scan(<<"584 ",Rest/binary>>)-> {ok, 584, Rest};
scan(<<"585 ",Rest/binary>>)-> {ok, 585, Rest};
scan(<<"586 ",Rest/binary>>)-> {ok, 586, Rest};
scan(<<"587 ",Rest/binary>>)-> {ok, 587, Rest};
scan(<<"588 ",Rest/binary>>)-> {ok, 588, Rest};
scan(<<"589 ",Rest/binary>>)-> {ok, 589, Rest};

scan(<<"590 ",Rest/binary>>)-> {ok, 590, Rest};
scan(<<"591 ",Rest/binary>>)-> {ok, 591, Rest};
scan(<<"592 ",Rest/binary>>)-> {ok, 592, Rest};
scan(<<"593 ",Rest/binary>>)-> {ok, 593, Rest};
scan(<<"594 ",Rest/binary>>)-> {ok, 594, Rest};
scan(<<"595 ",Rest/binary>>)-> {ok, 595, Rest};
scan(<<"596 ",Rest/binary>>)-> {ok, 596, Rest};
scan(<<"597 ",Rest/binary>>)-> {ok, 597, Rest};
scan(<<"598 ",Rest/binary>>)-> {ok, 598, Rest};
scan(<<"599 ",Rest/binary>>)-> {ok, 599, Rest};

scan(<<"600 ",Rest/binary>>)-> {ok, 600, Rest};
scan(<<"601 ",Rest/binary>>)-> {ok, 601, Rest};
scan(<<"602 ",Rest/binary>>)-> {ok, 602, Rest};
scan(<<"603 ",Rest/binary>>)-> {ok, 603, Rest};
scan(<<"604 ",Rest/binary>>)-> {ok, 604, Rest};
scan(<<"605 ",Rest/binary>>)-> {ok, 605, Rest};
scan(<<"606 ",Rest/binary>>)-> {ok, 606, Rest};
scan(<<"607 ",Rest/binary>>)-> {ok, 607, Rest};
scan(<<"608 ",Rest/binary>>)-> {ok, 608, Rest};
scan(<<"609 ",Rest/binary>>)-> {ok, 609, Rest};

scan(<<"610 ",Rest/binary>>)-> {ok, 610, Rest};
scan(<<"611 ",Rest/binary>>)-> {ok, 611, Rest};
scan(<<"612 ",Rest/binary>>)-> {ok, 612, Rest};
scan(<<"613 ",Rest/binary>>)-> {ok, 613, Rest};
scan(<<"614 ",Rest/binary>>)-> {ok, 614, Rest};
scan(<<"615 ",Rest/binary>>)-> {ok, 615, Rest};
scan(<<"616 ",Rest/binary>>)-> {ok, 616, Rest};
scan(<<"617 ",Rest/binary>>)-> {ok, 617, Rest};
scan(<<"618 ",Rest/binary>>)-> {ok, 618, Rest};
scan(<<"619 ",Rest/binary>>)-> {ok, 619, Rest};

scan(<<"620 ",Rest/binary>>)-> {ok, 620, Rest};
scan(<<"621 ",Rest/binary>>)-> {ok, 621, Rest};
scan(<<"622 ",Rest/binary>>)-> {ok, 622, Rest};
scan(<<"623 ",Rest/binary>>)-> {ok, 623, Rest};
scan(<<"624 ",Rest/binary>>)-> {ok, 624, Rest};
scan(<<"625 ",Rest/binary>>)-> {ok, 625, Rest};
scan(<<"626 ",Rest/binary>>)-> {ok, 626, Rest};
scan(<<"627 ",Rest/binary>>)-> {ok, 627, Rest};
scan(<<"628 ",Rest/binary>>)-> {ok, 628, Rest};
scan(<<"629 ",Rest/binary>>)-> {ok, 629, Rest};

scan(<<"630 ",Rest/binary>>)-> {ok, 630, Rest};
scan(<<"631 ",Rest/binary>>)-> {ok, 631, Rest};
scan(<<"632 ",Rest/binary>>)-> {ok, 632, Rest};
scan(<<"633 ",Rest/binary>>)-> {ok, 633, Rest};
scan(<<"634 ",Rest/binary>>)-> {ok, 634, Rest};
scan(<<"635 ",Rest/binary>>)-> {ok, 635, Rest};
scan(<<"636 ",Rest/binary>>)-> {ok, 636, Rest};
scan(<<"637 ",Rest/binary>>)-> {ok, 637, Rest};
scan(<<"638 ",Rest/binary>>)-> {ok, 638, Rest};
scan(<<"639 ",Rest/binary>>)-> {ok, 639, Rest};

scan(<<"640 ",Rest/binary>>)-> {ok, 640, Rest};
scan(<<"641 ",Rest/binary>>)-> {ok, 641, Rest};
scan(<<"642 ",Rest/binary>>)-> {ok, 642, Rest};
scan(<<"643 ",Rest/binary>>)-> {ok, 643, Rest};
scan(<<"644 ",Rest/binary>>)-> {ok, 644, Rest};
scan(<<"645 ",Rest/binary>>)-> {ok, 645, Rest};
scan(<<"646 ",Rest/binary>>)-> {ok, 646, Rest};
scan(<<"647 ",Rest/binary>>)-> {ok, 647, Rest};
scan(<<"648 ",Rest/binary>>)-> {ok, 648, Rest};
scan(<<"649 ",Rest/binary>>)-> {ok, 649, Rest};

scan(<<"650 ",Rest/binary>>)-> {ok, 650, Rest};
scan(<<"651 ",Rest/binary>>)-> {ok, 651, Rest};
scan(<<"652 ",Rest/binary>>)-> {ok, 652, Rest};
scan(<<"653 ",Rest/binary>>)-> {ok, 653, Rest};
scan(<<"654 ",Rest/binary>>)-> {ok, 654, Rest};
scan(<<"655 ",Rest/binary>>)-> {ok, 655, Rest};
scan(<<"656 ",Rest/binary>>)-> {ok, 656, Rest};
scan(<<"657 ",Rest/binary>>)-> {ok, 657, Rest};
scan(<<"658 ",Rest/binary>>)-> {ok, 658, Rest};
scan(<<"659 ",Rest/binary>>)-> {ok, 659, Rest};

scan(<<"660 ",Rest/binary>>)-> {ok, 660, Rest};
scan(<<"661 ",Rest/binary>>)-> {ok, 661, Rest};
scan(<<"662 ",Rest/binary>>)-> {ok, 662, Rest};
scan(<<"663 ",Rest/binary>>)-> {ok, 663, Rest};
scan(<<"664 ",Rest/binary>>)-> {ok, 664, Rest};
scan(<<"665 ",Rest/binary>>)-> {ok, 665, Rest};
scan(<<"666 ",Rest/binary>>)-> {ok, 666, Rest};
scan(<<"667 ",Rest/binary>>)-> {ok, 667, Rest};
scan(<<"668 ",Rest/binary>>)-> {ok, 668, Rest};
scan(<<"669 ",Rest/binary>>)-> {ok, 669, Rest};

scan(<<"670 ",Rest/binary>>)-> {ok, 670, Rest};
scan(<<"671 ",Rest/binary>>)-> {ok, 671, Rest};
scan(<<"672 ",Rest/binary>>)-> {ok, 672, Rest};
scan(<<"673 ",Rest/binary>>)-> {ok, 673, Rest};
scan(<<"674 ",Rest/binary>>)-> {ok, 674, Rest};
scan(<<"675 ",Rest/binary>>)-> {ok, 675, Rest};
scan(<<"676 ",Rest/binary>>)-> {ok, 676, Rest};
scan(<<"677 ",Rest/binary>>)-> {ok, 677, Rest};
scan(<<"678 ",Rest/binary>>)-> {ok, 678, Rest};
scan(<<"679 ",Rest/binary>>)-> {ok, 679, Rest};

scan(<<"680 ",Rest/binary>>)-> {ok, 680, Rest};
scan(<<"681 ",Rest/binary>>)-> {ok, 681, Rest};
scan(<<"682 ",Rest/binary>>)-> {ok, 682, Rest};
scan(<<"683 ",Rest/binary>>)-> {ok, 683, Rest};
scan(<<"684 ",Rest/binary>>)-> {ok, 684, Rest};
scan(<<"685 ",Rest/binary>>)-> {ok, 685, Rest};
scan(<<"686 ",Rest/binary>>)-> {ok, 686, Rest};
scan(<<"687 ",Rest/binary>>)-> {ok, 687, Rest};
scan(<<"688 ",Rest/binary>>)-> {ok, 688, Rest};
scan(<<"689 ",Rest/binary>>)-> {ok, 689, Rest};

scan(<<"690 ",Rest/binary>>)-> {ok, 690, Rest};
scan(<<"691 ",Rest/binary>>)-> {ok, 691, Rest};
scan(<<"692 ",Rest/binary>>)-> {ok, 692, Rest};
scan(<<"693 ",Rest/binary>>)-> {ok, 693, Rest};
scan(<<"694 ",Rest/binary>>)-> {ok, 694, Rest};
scan(<<"695 ",Rest/binary>>)-> {ok, 695, Rest};
scan(<<"696 ",Rest/binary>>)-> {ok, 696, Rest};
scan(<<"697 ",Rest/binary>>)-> {ok, 697, Rest};
scan(<<"698 ",Rest/binary>>)-> {ok, 698, Rest};
scan(<<"699 ",Rest/binary>>)-> {ok, 699, Rest};

scan(<<"700 ",Rest/binary>>)-> {ok, 700, Rest};
scan(<<"701 ",Rest/binary>>)-> {ok, 701, Rest};
scan(<<"702 ",Rest/binary>>)-> {ok, 702, Rest};
scan(<<"703 ",Rest/binary>>)-> {ok, 703, Rest};
scan(<<"704 ",Rest/binary>>)-> {ok, 704, Rest};
scan(<<"705 ",Rest/binary>>)-> {ok, 705, Rest};
scan(<<"706 ",Rest/binary>>)-> {ok, 706, Rest};
scan(<<"707 ",Rest/binary>>)-> {ok, 707, Rest};
scan(<<"708 ",Rest/binary>>)-> {ok, 708, Rest};
scan(<<"709 ",Rest/binary>>)-> {ok, 709, Rest};

scan(<<"710 ",Rest/binary>>)-> {ok, 710, Rest};
scan(<<"711 ",Rest/binary>>)-> {ok, 711, Rest};
scan(<<"712 ",Rest/binary>>)-> {ok, 712, Rest};
scan(<<"713 ",Rest/binary>>)-> {ok, 713, Rest};
scan(<<"714 ",Rest/binary>>)-> {ok, 714, Rest};
scan(<<"715 ",Rest/binary>>)-> {ok, 715, Rest};
scan(<<"716 ",Rest/binary>>)-> {ok, 716, Rest};
scan(<<"717 ",Rest/binary>>)-> {ok, 717, Rest};
scan(<<"718 ",Rest/binary>>)-> {ok, 718, Rest};
scan(<<"719 ",Rest/binary>>)-> {ok, 719, Rest};

scan(<<"720 ",Rest/binary>>)-> {ok, 720, Rest};
scan(<<"721 ",Rest/binary>>)-> {ok, 721, Rest};
scan(<<"722 ",Rest/binary>>)-> {ok, 722, Rest};
scan(<<"723 ",Rest/binary>>)-> {ok, 723, Rest};
scan(<<"724 ",Rest/binary>>)-> {ok, 724, Rest};
scan(<<"725 ",Rest/binary>>)-> {ok, 725, Rest};
scan(<<"726 ",Rest/binary>>)-> {ok, 726, Rest};
scan(<<"727 ",Rest/binary>>)-> {ok, 727, Rest};
scan(<<"728 ",Rest/binary>>)-> {ok, 728, Rest};
scan(<<"729 ",Rest/binary>>)-> {ok, 729, Rest};

scan(<<"730 ",Rest/binary>>)-> {ok, 730, Rest};
scan(<<"731 ",Rest/binary>>)-> {ok, 731, Rest};
scan(<<"732 ",Rest/binary>>)-> {ok, 732, Rest};
scan(<<"733 ",Rest/binary>>)-> {ok, 733, Rest};
scan(<<"734 ",Rest/binary>>)-> {ok, 734, Rest};
scan(<<"735 ",Rest/binary>>)-> {ok, 735, Rest};
scan(<<"736 ",Rest/binary>>)-> {ok, 736, Rest};
scan(<<"737 ",Rest/binary>>)-> {ok, 737, Rest};
scan(<<"738 ",Rest/binary>>)-> {ok, 738, Rest};
scan(<<"739 ",Rest/binary>>)-> {ok, 739, Rest};

scan(<<"740 ",Rest/binary>>)-> {ok, 740, Rest};
scan(<<"741 ",Rest/binary>>)-> {ok, 741, Rest};
scan(<<"742 ",Rest/binary>>)-> {ok, 742, Rest};
scan(<<"743 ",Rest/binary>>)-> {ok, 743, Rest};
scan(<<"744 ",Rest/binary>>)-> {ok, 744, Rest};
scan(<<"745 ",Rest/binary>>)-> {ok, 745, Rest};
scan(<<"746 ",Rest/binary>>)-> {ok, 746, Rest};
scan(<<"747 ",Rest/binary>>)-> {ok, 747, Rest};
scan(<<"748 ",Rest/binary>>)-> {ok, 748, Rest};
scan(<<"749 ",Rest/binary>>)-> {ok, 749, Rest};

scan(<<"750 ",Rest/binary>>)-> {ok, 750, Rest};
scan(<<"751 ",Rest/binary>>)-> {ok, 751, Rest};
scan(<<"752 ",Rest/binary>>)-> {ok, 752, Rest};
scan(<<"753 ",Rest/binary>>)-> {ok, 753, Rest};
scan(<<"754 ",Rest/binary>>)-> {ok, 754, Rest};
scan(<<"755 ",Rest/binary>>)-> {ok, 755, Rest};
scan(<<"756 ",Rest/binary>>)-> {ok, 756, Rest};
scan(<<"757 ",Rest/binary>>)-> {ok, 757, Rest};
scan(<<"758 ",Rest/binary>>)-> {ok, 758, Rest};
scan(<<"759 ",Rest/binary>>)-> {ok, 759, Rest};

scan(<<"760 ",Rest/binary>>)-> {ok, 760, Rest};
scan(<<"761 ",Rest/binary>>)-> {ok, 761, Rest};
scan(<<"762 ",Rest/binary>>)-> {ok, 762, Rest};
scan(<<"763 ",Rest/binary>>)-> {ok, 763, Rest};
scan(<<"764 ",Rest/binary>>)-> {ok, 764, Rest};
scan(<<"765 ",Rest/binary>>)-> {ok, 765, Rest};
scan(<<"766 ",Rest/binary>>)-> {ok, 766, Rest};
scan(<<"767 ",Rest/binary>>)-> {ok, 767, Rest};
scan(<<"768 ",Rest/binary>>)-> {ok, 768, Rest};
scan(<<"769 ",Rest/binary>>)-> {ok, 769, Rest};

scan(<<"770 ",Rest/binary>>)-> {ok, 770, Rest};
scan(<<"771 ",Rest/binary>>)-> {ok, 771, Rest};
scan(<<"772 ",Rest/binary>>)-> {ok, 772, Rest};
scan(<<"773 ",Rest/binary>>)-> {ok, 773, Rest};
scan(<<"774 ",Rest/binary>>)-> {ok, 774, Rest};
scan(<<"775 ",Rest/binary>>)-> {ok, 775, Rest};
scan(<<"776 ",Rest/binary>>)-> {ok, 776, Rest};
scan(<<"777 ",Rest/binary>>)-> {ok, 777, Rest};
scan(<<"778 ",Rest/binary>>)-> {ok, 778, Rest};
scan(<<"779 ",Rest/binary>>)-> {ok, 779, Rest};

scan(<<"780 ",Rest/binary>>)-> {ok, 780, Rest};
scan(<<"781 ",Rest/binary>>)-> {ok, 781, Rest};
scan(<<"782 ",Rest/binary>>)-> {ok, 782, Rest};
scan(<<"783 ",Rest/binary>>)-> {ok, 783, Rest};
scan(<<"784 ",Rest/binary>>)-> {ok, 784, Rest};
scan(<<"785 ",Rest/binary>>)-> {ok, 785, Rest};
scan(<<"786 ",Rest/binary>>)-> {ok, 786, Rest};
scan(<<"787 ",Rest/binary>>)-> {ok, 787, Rest};
scan(<<"788 ",Rest/binary>>)-> {ok, 788, Rest};
scan(<<"789 ",Rest/binary>>)-> {ok, 789, Rest};

scan(<<"790 ",Rest/binary>>)-> {ok, 790, Rest};
scan(<<"791 ",Rest/binary>>)-> {ok, 791, Rest};
scan(<<"792 ",Rest/binary>>)-> {ok, 792, Rest};
scan(<<"793 ",Rest/binary>>)-> {ok, 793, Rest};
scan(<<"794 ",Rest/binary>>)-> {ok, 794, Rest};
scan(<<"795 ",Rest/binary>>)-> {ok, 795, Rest};
scan(<<"796 ",Rest/binary>>)-> {ok, 796, Rest};
scan(<<"797 ",Rest/binary>>)-> {ok, 797, Rest};
scan(<<"798 ",Rest/binary>>)-> {ok, 798, Rest};
scan(<<"799 ",Rest/binary>>)-> {ok, 799, Rest};

scan(<<"800 ",Rest/binary>>)-> {ok, 800, Rest};
scan(<<"801 ",Rest/binary>>)-> {ok, 801, Rest};
scan(<<"802 ",Rest/binary>>)-> {ok, 802, Rest};
scan(<<"803 ",Rest/binary>>)-> {ok, 803, Rest};
scan(<<"804 ",Rest/binary>>)-> {ok, 804, Rest};
scan(<<"805 ",Rest/binary>>)-> {ok, 805, Rest};
scan(<<"806 ",Rest/binary>>)-> {ok, 806, Rest};
scan(<<"807 ",Rest/binary>>)-> {ok, 807, Rest};
scan(<<"808 ",Rest/binary>>)-> {ok, 808, Rest};
scan(<<"809 ",Rest/binary>>)-> {ok, 809, Rest};

scan(<<"810 ",Rest/binary>>)-> {ok, 810, Rest};
scan(<<"811 ",Rest/binary>>)-> {ok, 811, Rest};
scan(<<"812 ",Rest/binary>>)-> {ok, 812, Rest};
scan(<<"813 ",Rest/binary>>)-> {ok, 813, Rest};
scan(<<"814 ",Rest/binary>>)-> {ok, 814, Rest};
scan(<<"815 ",Rest/binary>>)-> {ok, 815, Rest};
scan(<<"816 ",Rest/binary>>)-> {ok, 816, Rest};
scan(<<"817 ",Rest/binary>>)-> {ok, 817, Rest};
scan(<<"818 ",Rest/binary>>)-> {ok, 818, Rest};
scan(<<"819 ",Rest/binary>>)-> {ok, 819, Rest};

scan(<<"820 ",Rest/binary>>)-> {ok, 820, Rest};
scan(<<"821 ",Rest/binary>>)-> {ok, 821, Rest};
scan(<<"822 ",Rest/binary>>)-> {ok, 822, Rest};
scan(<<"823 ",Rest/binary>>)-> {ok, 823, Rest};
scan(<<"824 ",Rest/binary>>)-> {ok, 824, Rest};
scan(<<"825 ",Rest/binary>>)-> {ok, 825, Rest};
scan(<<"826 ",Rest/binary>>)-> {ok, 826, Rest};
scan(<<"827 ",Rest/binary>>)-> {ok, 827, Rest};
scan(<<"828 ",Rest/binary>>)-> {ok, 828, Rest};
scan(<<"829 ",Rest/binary>>)-> {ok, 829, Rest};

scan(<<"830 ",Rest/binary>>)-> {ok, 830, Rest};
scan(<<"831 ",Rest/binary>>)-> {ok, 831, Rest};
scan(<<"832 ",Rest/binary>>)-> {ok, 832, Rest};
scan(<<"833 ",Rest/binary>>)-> {ok, 833, Rest};
scan(<<"834 ",Rest/binary>>)-> {ok, 834, Rest};
scan(<<"835 ",Rest/binary>>)-> {ok, 835, Rest};
scan(<<"836 ",Rest/binary>>)-> {ok, 836, Rest};
scan(<<"837 ",Rest/binary>>)-> {ok, 837, Rest};
scan(<<"838 ",Rest/binary>>)-> {ok, 838, Rest};
scan(<<"839 ",Rest/binary>>)-> {ok, 839, Rest};

scan(<<"840 ",Rest/binary>>)-> {ok, 840, Rest};
scan(<<"841 ",Rest/binary>>)-> {ok, 841, Rest};
scan(<<"842 ",Rest/binary>>)-> {ok, 842, Rest};
scan(<<"843 ",Rest/binary>>)-> {ok, 843, Rest};
scan(<<"844 ",Rest/binary>>)-> {ok, 844, Rest};
scan(<<"845 ",Rest/binary>>)-> {ok, 845, Rest};
scan(<<"846 ",Rest/binary>>)-> {ok, 846, Rest};
scan(<<"847 ",Rest/binary>>)-> {ok, 847, Rest};
scan(<<"848 ",Rest/binary>>)-> {ok, 848, Rest};
scan(<<"849 ",Rest/binary>>)-> {ok, 849, Rest};

scan(<<"850 ",Rest/binary>>)-> {ok, 850, Rest};
scan(<<"851 ",Rest/binary>>)-> {ok, 851, Rest};
scan(<<"852 ",Rest/binary>>)-> {ok, 852, Rest};
scan(<<"853 ",Rest/binary>>)-> {ok, 853, Rest};
scan(<<"854 ",Rest/binary>>)-> {ok, 854, Rest};
scan(<<"855 ",Rest/binary>>)-> {ok, 855, Rest};
scan(<<"856 ",Rest/binary>>)-> {ok, 856, Rest};
scan(<<"857 ",Rest/binary>>)-> {ok, 857, Rest};
scan(<<"858 ",Rest/binary>>)-> {ok, 858, Rest};
scan(<<"859 ",Rest/binary>>)-> {ok, 859, Rest};

scan(<<"860 ",Rest/binary>>)-> {ok, 860, Rest};
scan(<<"861 ",Rest/binary>>)-> {ok, 861, Rest};
scan(<<"862 ",Rest/binary>>)-> {ok, 862, Rest};
scan(<<"863 ",Rest/binary>>)-> {ok, 863, Rest};
scan(<<"864 ",Rest/binary>>)-> {ok, 864, Rest};
scan(<<"865 ",Rest/binary>>)-> {ok, 865, Rest};
scan(<<"866 ",Rest/binary>>)-> {ok, 866, Rest};
scan(<<"867 ",Rest/binary>>)-> {ok, 867, Rest};
scan(<<"868 ",Rest/binary>>)-> {ok, 868, Rest};
scan(<<"869 ",Rest/binary>>)-> {ok, 869, Rest};

scan(<<"870 ",Rest/binary>>)-> {ok, 870, Rest};
scan(<<"871 ",Rest/binary>>)-> {ok, 871, Rest};
scan(<<"872 ",Rest/binary>>)-> {ok, 872, Rest};
scan(<<"873 ",Rest/binary>>)-> {ok, 873, Rest};
scan(<<"874 ",Rest/binary>>)-> {ok, 874, Rest};
scan(<<"875 ",Rest/binary>>)-> {ok, 875, Rest};
scan(<<"876 ",Rest/binary>>)-> {ok, 876, Rest};
scan(<<"877 ",Rest/binary>>)-> {ok, 877, Rest};
scan(<<"878 ",Rest/binary>>)-> {ok, 878, Rest};
scan(<<"879 ",Rest/binary>>)-> {ok, 879, Rest};

scan(<<"880 ",Rest/binary>>)-> {ok, 880, Rest};
scan(<<"881 ",Rest/binary>>)-> {ok, 881, Rest};
scan(<<"882 ",Rest/binary>>)-> {ok, 882, Rest};
scan(<<"883 ",Rest/binary>>)-> {ok, 883, Rest};
scan(<<"884 ",Rest/binary>>)-> {ok, 884, Rest};
scan(<<"885 ",Rest/binary>>)-> {ok, 885, Rest};
scan(<<"886 ",Rest/binary>>)-> {ok, 886, Rest};
scan(<<"887 ",Rest/binary>>)-> {ok, 887, Rest};
scan(<<"888 ",Rest/binary>>)-> {ok, 888, Rest};
scan(<<"889 ",Rest/binary>>)-> {ok, 889, Rest};

scan(<<"890 ",Rest/binary>>)-> {ok, 890, Rest};
scan(<<"891 ",Rest/binary>>)-> {ok, 891, Rest};
scan(<<"892 ",Rest/binary>>)-> {ok, 892, Rest};
scan(<<"893 ",Rest/binary>>)-> {ok, 893, Rest};
scan(<<"894 ",Rest/binary>>)-> {ok, 894, Rest};
scan(<<"895 ",Rest/binary>>)-> {ok, 895, Rest};
scan(<<"896 ",Rest/binary>>)-> {ok, 896, Rest};
scan(<<"897 ",Rest/binary>>)-> {ok, 897, Rest};
scan(<<"898 ",Rest/binary>>)-> {ok, 898, Rest};
scan(<<"899 ",Rest/binary>>)-> {ok, 899, Rest};

scan(<<"900 ",Rest/binary>>)-> {ok, 900, Rest};
scan(<<"901 ",Rest/binary>>)-> {ok, 901, Rest};
scan(<<"902 ",Rest/binary>>)-> {ok, 902, Rest};
scan(<<"903 ",Rest/binary>>)-> {ok, 903, Rest};
scan(<<"904 ",Rest/binary>>)-> {ok, 904, Rest};
scan(<<"905 ",Rest/binary>>)-> {ok, 905, Rest};
scan(<<"906 ",Rest/binary>>)-> {ok, 906, Rest};
scan(<<"907 ",Rest/binary>>)-> {ok, 907, Rest};
scan(<<"908 ",Rest/binary>>)-> {ok, 908, Rest};
scan(<<"909 ",Rest/binary>>)-> {ok, 909, Rest};

scan(<<"910 ",Rest/binary>>)-> {ok, 910, Rest};
scan(<<"911 ",Rest/binary>>)-> {ok, 911, Rest};
scan(<<"912 ",Rest/binary>>)-> {ok, 912, Rest};
scan(<<"913 ",Rest/binary>>)-> {ok, 913, Rest};
scan(<<"914 ",Rest/binary>>)-> {ok, 914, Rest};
scan(<<"915 ",Rest/binary>>)-> {ok, 915, Rest};
scan(<<"916 ",Rest/binary>>)-> {ok, 916, Rest};
scan(<<"917 ",Rest/binary>>)-> {ok, 917, Rest};
scan(<<"918 ",Rest/binary>>)-> {ok, 918, Rest};
scan(<<"919 ",Rest/binary>>)-> {ok, 919, Rest};

scan(<<"920 ",Rest/binary>>)-> {ok, 920, Rest};
scan(<<"921 ",Rest/binary>>)-> {ok, 921, Rest};
scan(<<"922 ",Rest/binary>>)-> {ok, 922, Rest};
scan(<<"923 ",Rest/binary>>)-> {ok, 923, Rest};
scan(<<"924 ",Rest/binary>>)-> {ok, 924, Rest};
scan(<<"925 ",Rest/binary>>)-> {ok, 925, Rest};
scan(<<"926 ",Rest/binary>>)-> {ok, 926, Rest};
scan(<<"927 ",Rest/binary>>)-> {ok, 927, Rest};
scan(<<"928 ",Rest/binary>>)-> {ok, 928, Rest};
scan(<<"929 ",Rest/binary>>)-> {ok, 929, Rest};

scan(<<"930 ",Rest/binary>>)-> {ok, 930, Rest};
scan(<<"931 ",Rest/binary>>)-> {ok, 931, Rest};
scan(<<"932 ",Rest/binary>>)-> {ok, 932, Rest};
scan(<<"933 ",Rest/binary>>)-> {ok, 933, Rest};
scan(<<"934 ",Rest/binary>>)-> {ok, 934, Rest};
scan(<<"935 ",Rest/binary>>)-> {ok, 935, Rest};
scan(<<"936 ",Rest/binary>>)-> {ok, 936, Rest};
scan(<<"937 ",Rest/binary>>)-> {ok, 937, Rest};
scan(<<"938 ",Rest/binary>>)-> {ok, 938, Rest};
scan(<<"939 ",Rest/binary>>)-> {ok, 939, Rest};

scan(<<"940 ",Rest/binary>>)-> {ok, 940, Rest};
scan(<<"941 ",Rest/binary>>)-> {ok, 941, Rest};
scan(<<"942 ",Rest/binary>>)-> {ok, 942, Rest};
scan(<<"943 ",Rest/binary>>)-> {ok, 943, Rest};
scan(<<"944 ",Rest/binary>>)-> {ok, 944, Rest};
scan(<<"945 ",Rest/binary>>)-> {ok, 945, Rest};
scan(<<"946 ",Rest/binary>>)-> {ok, 946, Rest};
scan(<<"947 ",Rest/binary>>)-> {ok, 947, Rest};
scan(<<"948 ",Rest/binary>>)-> {ok, 948, Rest};
scan(<<"949 ",Rest/binary>>)-> {ok, 949, Rest};

scan(<<"950 ",Rest/binary>>)-> {ok, 950, Rest};
scan(<<"951 ",Rest/binary>>)-> {ok, 951, Rest};
scan(<<"952 ",Rest/binary>>)-> {ok, 952, Rest};
scan(<<"953 ",Rest/binary>>)-> {ok, 953, Rest};
scan(<<"954 ",Rest/binary>>)-> {ok, 954, Rest};
scan(<<"955 ",Rest/binary>>)-> {ok, 955, Rest};
scan(<<"956 ",Rest/binary>>)-> {ok, 956, Rest};
scan(<<"957 ",Rest/binary>>)-> {ok, 957, Rest};
scan(<<"958 ",Rest/binary>>)-> {ok, 958, Rest};
scan(<<"959 ",Rest/binary>>)-> {ok, 959, Rest};

scan(<<"960 ",Rest/binary>>)-> {ok, 960, Rest};
scan(<<"961 ",Rest/binary>>)-> {ok, 961, Rest};
scan(<<"962 ",Rest/binary>>)-> {ok, 962, Rest};
scan(<<"963 ",Rest/binary>>)-> {ok, 963, Rest};
scan(<<"964 ",Rest/binary>>)-> {ok, 964, Rest};
scan(<<"965 ",Rest/binary>>)-> {ok, 965, Rest};
scan(<<"966 ",Rest/binary>>)-> {ok, 966, Rest};
scan(<<"967 ",Rest/binary>>)-> {ok, 967, Rest};
scan(<<"968 ",Rest/binary>>)-> {ok, 968, Rest};
scan(<<"969 ",Rest/binary>>)-> {ok, 969, Rest};

scan(<<"970 ",Rest/binary>>)-> {ok, 970, Rest};
scan(<<"971 ",Rest/binary>>)-> {ok, 971, Rest};
scan(<<"972 ",Rest/binary>>)-> {ok, 972, Rest};
scan(<<"973 ",Rest/binary>>)-> {ok, 973, Rest};
scan(<<"974 ",Rest/binary>>)-> {ok, 974, Rest};
scan(<<"975 ",Rest/binary>>)-> {ok, 975, Rest};
scan(<<"976 ",Rest/binary>>)-> {ok, 976, Rest};
scan(<<"977 ",Rest/binary>>)-> {ok, 977, Rest};
scan(<<"978 ",Rest/binary>>)-> {ok, 978, Rest};
scan(<<"979 ",Rest/binary>>)-> {ok, 979, Rest};

scan(<<"980 ",Rest/binary>>)-> {ok, 980, Rest};
scan(<<"981 ",Rest/binary>>)-> {ok, 981, Rest};
scan(<<"982 ",Rest/binary>>)-> {ok, 982, Rest};
scan(<<"983 ",Rest/binary>>)-> {ok, 983, Rest};
scan(<<"984 ",Rest/binary>>)-> {ok, 984, Rest};
scan(<<"985 ",Rest/binary>>)-> {ok, 985, Rest};
scan(<<"986 ",Rest/binary>>)-> {ok, 986, Rest};
scan(<<"987 ",Rest/binary>>)-> {ok, 987, Rest};
scan(<<"988 ",Rest/binary>>)-> {ok, 988, Rest};
scan(<<"989 ",Rest/binary>>)-> {ok, 989, Rest};

scan(<<"990 ",Rest/binary>>)-> {ok, 990, Rest};
scan(<<"991 ",Rest/binary>>)-> {ok, 991, Rest};
scan(<<"992 ",Rest/binary>>)-> {ok, 992, Rest};
scan(<<"993 ",Rest/binary>>)-> {ok, 993, Rest};
scan(<<"994 ",Rest/binary>>)-> {ok, 994, Rest};
scan(<<"995 ",Rest/binary>>)-> {ok, 995, Rest};
scan(<<"996 ",Rest/binary>>)-> {ok, 996, Rest};
scan(<<"997 ",Rest/binary>>)-> {ok, 997, Rest};
scan(<<"998 ",Rest/binary>>)-> {ok, 998, Rest};
scan(<<"999 ",Rest/binary>>)-> {ok, 999, Rest};

scan(<<"1000 ",Rest/binary>>)-> {ok, 1000, Rest};
scan(<<"1001 ",Rest/binary>>)-> {ok, 1001, Rest};
scan(<<"1002 ",Rest/binary>>)-> {ok, 1002, Rest};
scan(<<"1003 ",Rest/binary>>)-> {ok, 1003, Rest};
scan(<<"1004 ",Rest/binary>>)-> {ok, 1004, Rest};
scan(<<"1005 ",Rest/binary>>)-> {ok, 1005, Rest};
scan(<<"1006 ",Rest/binary>>)-> {ok, 1006, Rest};
scan(<<"1007 ",Rest/binary>>)-> {ok, 1007, Rest};
scan(<<"1008 ",Rest/binary>>)-> {ok, 1008, Rest};
scan(<<"1009 ",Rest/binary>>)-> {ok, 1009, Rest};

scan(<<"1010 ",Rest/binary>>)-> {ok, 1010, Rest};
scan(<<"1011 ",Rest/binary>>)-> {ok, 1011, Rest};
scan(<<"1012 ",Rest/binary>>)-> {ok, 1012, Rest};
scan(<<"1013 ",Rest/binary>>)-> {ok, 1013, Rest};
scan(<<"1014 ",Rest/binary>>)-> {ok, 1014, Rest};
scan(<<"1015 ",Rest/binary>>)-> {ok, 1015, Rest};
scan(<<"1016 ",Rest/binary>>)-> {ok, 1016, Rest};
scan(<<"1017 ",Rest/binary>>)-> {ok, 1017, Rest};
scan(<<"1018 ",Rest/binary>>)-> {ok, 1018, Rest};
scan(<<"1019 ",Rest/binary>>)-> {ok, 1019, Rest};

scan(<<"1020 ",Rest/binary>>)-> {ok, 1020, Rest};
scan(<<"1021 ",Rest/binary>>)-> {ok, 1021, Rest};
scan(<<"1022 ",Rest/binary>>)-> {ok, 1022, Rest};
scan(<<"1023 ",Rest/binary>>)-> {ok, 1023, Rest};
scan(<<"1024 ",Rest/binary>>)-> {ok, 1024, Rest};

%% We've at the end of the stream and we were in the middle of checking numbers
scan(<<N1>>) when ?NON_ZERO_NUMBER(N1) -> continue;
scan(<<N1,_N2>>) when ?NON_ZERO_NUMBER(N1) andalso ?NUMBER(_N2) -> continue;
scan(<<N1,_N2,_N3>>) when ?NON_ZERO_NUMBER(N1) andalso ?NUMBER(_N2) andalso ?NUMBER(_N3) -> continue;

%% It's a number but it's too long so we're just going to toss this frame
scan(<<N1,_/binary>>) when ?NON_ZERO_NUMBER(N1) -> eos;

scan(<<>>)-> eos;

%% We don't understand it
scan(<<_, Rest/binary>>)-> scan(Rest).

%%%%
%% analyze the frame
%%%%

%% We are at the end of the stream and have a perfect match
analyze({ok, Length, Rest}) when byte_size(Rest) =:= Length ->
  {ok, Rest, <<>>};
%% There's still more in the buffer so we can do some checks
analyze({ok, Length, Rest}) when byte_size(Rest) > Length ->
  %% check the next frame to see if we have a valid frame here
  <<Frame:Length/binary,Rest2/binary>> = Rest,
  case Rest2 of
    %% The next frame starts with a digit; in most cases this means we have
    %% a valid frame
    <<$1,_/binary>> ->
      {ok, Frame, Rest2};
    <<$2,_/binary>> ->
      {ok, Frame, Rest2};
    <<$3,_/binary>> ->
      {ok, Frame, Rest2};
    <<$4,_/binary>> ->
      {ok, Frame, Rest2};
    <<$5,_/binary>> ->
      {ok, Frame, Rest2};
    <<$6,_/binary>> ->
      {ok, Frame, Rest2};
    <<$7,_/binary>> ->
      {ok, Frame, Rest2};
    <<$8,_/binary>> ->
      {ok, Frame, Rest2};
    <<$9,_/binary>> ->
      {ok, Frame, Rest2};

    %% The next "frame" after `Length` doesn't start with a number so we're
    %% probably not looking at a valid frame here; move on by skipping this
    %% `Length`
    _ ->
      <<_,Rest3/binary>> = Rest2,
      frame(Rest3)
  end;
%% We need more in the buffer to understand this frame
analyze({ok, _, _}) ->
  continue;
%% Pass on the result if we don't understand it
analyze(Result) ->
  Result.
