# resfun-panelists-code
This repository contains the code for the resfun-panelists project.

The code in scrapper-1 scrapes the panel names along with their respective panelists and subsequently saves the data as a csv file. The webpage scrapped here is a 2021 version of https://gacr.cz/o-ga-cr/poradni-organy/panely/, as archived on the wayback machine.

The code in scrapper-2 is almost identical and achieves the same with the 2022 version of the same webpage.

Scrapper-3 extracts and processes data from 2008 version of the website http://pala.gacr.cas.cz/wordpress/ pages 91 to 95 (sub-branch data).

Scrapper-4 extracts and saves panelists data (sub-branch) from 2007 version of the website http://pala.gacr.cas.cz/wordpress as archived by the waybackmachine.

Post-processor fetches the extracted data, processes them to specifications as well as standardises them. Followed by merging them all into one dataframe and saving that dataframe as a csv file.

The main file brings together all the scrappers and the post processors and runs them.
