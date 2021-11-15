# table-contest (Repository for the RStudio Table Contest of 2021)

The app is hosted at: https://icarob.shinyapps.io/BGGexplorer/

![App preview](https://github.com/IcaroBernardes/table-contest/blob/main/preview.png)

This app is a companion to explore the games registred at the Board Game Geek site (https://boardgamegeek.com). Data was collected from Dilini Samarasinghe, July 5, 2021, "BoardGameGeek Dataset on Board Games", IEEE Dataport, doi: https://dx.doi.org/10.21227/9g61-bs59.

The app has three sections, a control panel, a list of games (table) and a scatter plot.

* Control panel:
  + You can search games by name in the first input ("Name of game"). The input is case-sensitive and matches subsets of strings;
  + Inputs from "Year of publication" till "# of game owners" are numerical by default. You can switch each of them to a categorical selection by flipping the "type" switch. When the switch is flipped, both the variable and the input selector change;
  + The last input ("Domains") can be turned on and off using the "filtering" switch.
  + Filters can be reset by clicking the "reset" buttons.

* List of games:
  + Shows the data from the filtered games. Responds only to the inputs on the panel;
  + There are checkboxes in the first column. Selected games are highlighted in the scatter plot;
  + Selections are cleared if any alteration is made to the inputs.

* Scatter plot:
  + Shows the Rating and Complexity average of each game as points. Responds to both the inputs on the panel and the selections on the table;
  + Selected games at the table are highlighted. They become red and more opaque;
  + If the "Rating Average" or "Complexity Average" are converted to categorical, some jittery is added to the points.

To facilitate navigation, the minus icons hide/show sections of the app.
