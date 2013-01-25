(* Test File for "calendar.sml" *)
val d1  = (1988, 9, 29);
val d2  = (2001, 9, 11);
val d3  = (2013, 1, 14);
val d4  = (1997, 7, 17);
val ds  = [d1, d2, d3, d4];
val ms1 = [1, 9];
val ms2 = [9, 11, 6];
val ms3 = [2, 3, 4, 6, 8, 10];
val ms4 = [10, 7];

val check11 = is_older(d1, d2) = true;
val check12 = is_older(d2, d1) = false;
val check13 = is_older(d1, d1) = false;
val check14 = is_older(d3, d2) = false;
val check15 = is_older(d2, d3) = true;
val check16 = is_older(d4, d1) = false;

val check21 = number_in_month(ds, 9) = 2;
val check22 = number_in_month(ds, 1) = 1;
val check23 = number_in_month(ds, 3) = 0;

val check31 = number_in_months(ds, ms1) = 3;
val check32 = number_in_months(ds, ms2) = 2;
val check33 = number_in_months(ds, ms3) = 0;

val check41 = dates_in_month(ds, 9) = [d1, d2];
val check42 = dates_in_month(ds, 1) = [d3];
val check43 = dates_in_month(ds, 3) = [ ];

val check51 = dates_in_months(ds, ms1) = [d3, d1, d2];
val check52 = dates_in_months(ds, ms2) = [d1, d2];
val check53 = dates_in_months(ds, ms3) = [ ];
val check54 = dates_in_months(ds, ms4) = [d4];

val ss1     = ["one", "two", "three", "four", "five"];
val check61 = get_nth(ss1, 2) = "two";
val check62 = get_nth(ss1, 1) = "one";

val check71 = date_to_string(d1) = "September 29, 1988";
val check72 = date_to_string(d2) = "September 11, 2001";
val check73 = date_to_string(d3) = "January 14, 2013";
val check74 = date_to_string(d4) = "July 17, 1997";

val numbers = [1, 6, 9, 4, 2, 19];
val check81 = number_before_reaching_sum (8, numbers)  = 2;
val check82 = number_before_reaching_sum (17, numbers) = 3;
val check83 = number_before_reaching_sum (21, numbers) = 4;
val check84 = number_before_reaching_sum (1, numbers)  = 0;

val check91 = what_month (10)  = 1;
val check92 = what_month (360) = 12;
val check93 = what_month (150) = 5;
val check94 = what_month (290) = 10;
val check95 = what_month (60)  = 3; (* no leap years *)

val check101 = month_range(29,34)   = [1, 1, 1, 2, 2, 2];
val check102 = month_range(20,19)   = [ ];
val check103 = month_range(101,101) = [4];
val check104 = month_range(304,305) = [10, 11];

val check111 = oldest([ ])  = NONE;
val check112 = oldest([d2]) = SOME(d2);
val check113 = oldest(ds)   = SOME(d1);

val check121 = number_in_months_challenge(ds, ms1) = number_in_months(ds, ms1);
val check122 = number_in_months_challenge(ds, ms2) = number_in_months(ds, ms2);
val check123 = number_in_months_challenge(ds, ms3) = number_in_months(ds, ms3);
val check124 = number_in_months_challenge(ds, ms1 @ ms1) = number_in_months(ds, ms1);
val check125 = number_in_months_challenge(ds, ms2 @ ms2) = number_in_months(ds, ms2);
val check126 = number_in_months_challenge(ds, ms3 @ ms3) = number_in_months(ds, ms3);

val check127 = dates_in_months_challenge(ds, ms1) = dates_in_months(ds, ms1);
val check128 = dates_in_months_challenge(ds, ms2) = dates_in_months(ds, ms2);
val check129 = dates_in_months_challenge(ds, ms3) = dates_in_months(ds, ms3);
val check12A = dates_in_months_challenge(ds, ms4) = dates_in_months(ds, ms4);
val check12B = dates_in_months_challenge(ds, ms1 @ ms1)=dates_in_months(ds, ms1);
val check12C = dates_in_months_challenge(ds, ms2 @ ms2)=dates_in_months(ds, ms2);
val check12D = dates_in_months_challenge(ds, ms3 @ ms3)=dates_in_months(ds, ms3);
val check12E = dates_in_months_challenge(ds, ms4 @ ms4)=dates_in_months(ds, ms4);

val check133 = reasonable_date(d1) = true;
val check134 = reasonable_date(d2) = true;
val check135 = reasonable_date(d3) = true;
val check136 = reasonable_date(d4) = true;
val check137 = reasonable_date((~10, 1, 1))   =false;
val check138 = reasonable_date((1900, 2, 29)) = false;
val check139 = reasonable_date((1904, 2, 29)) = true;
