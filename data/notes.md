orig_test_data.csv comes from the original file pr_usvi1623_fish17sppLH_ARdat2v2_rug_tst.csv. Copied 11/8/25

orig_test_25result.csv is the final result of running ProcessDev.R generate_bins(25) and is a base good known result.

orig_test_40logit_result.csv is the final result of running ProcessDev.R generate_bins(40, TRUE) and is a base known good result.

PrimaryGroupExtractor results for each spp are contained in primarygroupextractions table in postgres


test_data_basic from postgres.

select generate_series(0, 90, 10),
generate_series(9,99,10);

insert into test_data (measure)
SELECT floor(random() * 100) + 1 AS random_integer 
FROM generate_series(1, 200);

update test_data set (group_name) = (select 'tst_spp')ß

select group_name, measure from test_data

Fill sparse values
Transform the initial data frame so that missing values are filled within the category, e.g. a dataset that went 23, 23, 25, 28 would have zero-valued rows entered for 26 and 27.
This has to be done within each group.

Need test data that has missing group values at count boundaries to check for min/max/mid.
