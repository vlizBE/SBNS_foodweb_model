# Southern Bight of the North Sea Food Web Model
An Ecopath with Ecosim food web model, accompanied by R scripts to process and analyse the data.

## Table of Contents
- [Overview](#overview)
- [Features](#features)
- [Software](#Software)
- [Installation](#installation)
- [Usage](#usage)
- [Scientific output](#Scientific-output)
- [Data availability](#Data-availability)
- [License](#license)
- [Acknowledgements](#Acknowledgements)

## Overview
The food web model of the Southern Bight of the North Sea (1991–2023) developed in this study features a trophic structure composed of 43 functional groups, spanning all trophic levels. These include detritus (such as organic matter and discards), primary producers, invertebrates, commercially important fish species (for example, sole, _Solea solea_), and top predators (including marine mammals and birds). Some functional groups are multi-stanza, meaning they are divided into age classes to reflect shifts in ecological roles across different life stages. The model incorporates interactions with both commercial and recreational fisheries. Nine commercial fishing fleets are differentiated based on gear types, following the classification by the Scientific, Technical and Economic Committee for Fisheries (STECF, 2017). Additionally, five recreational fishing techniques, as identified by Verleye et al. (2022) in their study of Belgian recreational fisheries, are included. Fishing effort and Biomass and catch time series data (1991–2023) are included to fit the model. 

For a detailed overview of the functional groups and fisheries fleets, please take a look at the technical report (Pint et al., 2026).

## Features
- Ecopath with Ecosim model.
- A temporal-dynamic food web model.
- Outputs include time series (e.g., biomass, catch), Monte-Carlo results, and ecological indicators.
- Configurable for different scenarios.

## Software
- Ecopath with Ecosim
- R

## Installation
Ecopath with Ecosim software is available via https://ecopath.org/.

## Usage
The R scripts are either used to process data to import them into the EwE model or to analyse the results exported from the EwE model (including Monte Carlo results). 
The food web model developed in EwE was used to assess the historical dynamics of the Southern Bight of the North Sea food web. It can be used to run future scenarios and to assess management strategies.

## Scientific output
- Pint, S.; Stevens, M.; Musimwa, R.; Standaert, W.; De Troch, M.; van Oevelen, D.; Heymans, J.J.; Everaert, G. (2024) A food web model of the Southern Bight of the   North   Sea. Ocean & Coastal Management. Volume 255. 107256. ISSN 0964-5691. https://doi.org/10.1016/j.ocecoaman.2024.107256.
- Pint S., Lorré D., Stevens M., De Troch M., Heymans S., van   Oevelen D., Musimwa R., Everaert G. (in prep.) Historical dynamics of the Southern Bight of the        North Sea (1991–2023): A food web modelling approach.
- Pint, S.; Stevens, M.; De Troch, M.; van Oevelen, D.; Heymans, J.J.; Everaert, G. (2024). Ecopath model of the Southern Bight of the North Sea. Version 3.           Flanders Marine Institute (VLIZ): Ostend. 37 pp. https://dx.doi.org/10.48470/74.
- Pint, S.; Lorré, D.; Stevens, M.; De Troch, M.; van Oevelen, D.; Heymans, J.J.; Everaert, G. (in prep). Ecopath model of the Southern Bight of the North Sea.        Version 4. Flanders Marine Institute (VLIZ): Ostend.

## Data availability
- INCLUDE RECENT RESULTS LINK
- The input data, Rscripts and Ecopath model can be accessed by https://doi.org/10.14284/668 and should be cited as follows: Pint, S.; Stevens, M.; De Troch, M.;      van Oevelen, D.; Heymans, J.J.; Everaert, G.; Flanders Marine Institute; Ghent University; Royal Netherlands Institute for Sea Research; University of the           Highlands and Islands; (2024): Data - Ecopath model of the Southern Bight of the North Sea. Marine Data Archive. https://doi.org/10.14284/668

## License
This project is licensed under the Creative Commons CC-BY 4.0 license.

## Acknowledgments
The production of this work has been supported by the European Union (Grant agreement ID 101112823).



---

For questions or further information, please contact the project maintainer at [steven.pint@vliz.be] or [Dries.Lorre@UGent.be].

---






