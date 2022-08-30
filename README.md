# Norberg_2020

## Repository of paper Identification of Genetic Markers Linked to Alfalfa Yield, Height and Fall Dormancy

Phenotypic data of 1_MSC, 2_DM, 3_Height, 4_Yield, and 5_FD were collected in three locations: WA, OR and ID in 2018, 2019 and 2020. The same accessions were genotyped using genotyping-by-sequencing followed by SNP calling and filtering. Genome-wide association studies were performed to identify genetic markers associated with alfalfa yield, height and fall dormancy.

There are three locations, three years (multiple cuts), and five traits:

Table 1. Harvest interval by year and location. Three-cut system was applied in 2018 in all locations. Four-cut system was applied in ID 2019 and in OR 2019 and 2020. Five-cut system was applied in WA in 2019 and 2020.

| Year  |    ID   |    OR   |     WA    |
|:-----:|:-------:|:-------:|:---------:|
|  2018 | 1,2,3,− | 1,2,3,− | 1,2,3,−,− |
|  2019 | 1,2,3,4 | 1,2,3,4 | 1,2,3,4,5 |
|  2020 |       − | 1,2,3,4 | 1,2,3,4,5 |

Table 2. Traits collected in three locations; Idaho (ID), Oregon (OR), and Washington (WA) from 2018 to 2020, multiple times by year. Traits tested were maturity measured as mean stage by count (MSC), dry matter (DM), plant height (PH), yield (Yi), and fall dormancy (FD).

| Dataset  | MSC | DM | PH | FD | Yi | Total |
|----------|:---:|:--:|:--:|:--:|:--:|:-----:|
| Total_ID | 2   | 2  | 1  | 1  | 9  | 15    |
| Total_OR | 3   | 3  | 6  | 3  | 14 | 29    |
| Total_WA | 8   | 3  | 12 | 2  | 16 | 41    |
| Total    | 13  | 8  | 19 | 6  | 39 | 85    |

It is possible to generate an analysis to obtain BLUP values (ST0 and ST1) to run GWAS:

- Forage yield Stage BLUEs Measures.
- Single cut 1_stage 22.
- Single year 2_stage 6.
- Single loc 3_stage 3.
- All 4_stage 1.

It is important to accurately evaluate the developmental stage of forage crops in order to precisely establish the appropriate harvest time and to optimize their yield, nutritive value and persistence. This guide illustrates and explains the developmental stages of timothy and alfalfa, the two main forage species cultivated in Quebec. It also describes two methods used to determine the mean stage of such forage: the Mean Stage by Count (MSC) and the Mean Stage by Weight (MSW).

## Pearson Correlation

There are some problematic env with yield:

- Yi_ID_2018_1
- Yi_ID_2018_2
- Yi_OR_2018_1
- Yi_OR_2018_2
- Yi_OR_2019_1
- Yi_OR_2020_1
- Yi_WA_2018_1
