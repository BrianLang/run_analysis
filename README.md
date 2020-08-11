Shiny App to visualize data from gps-enabled watches.

The goal is to create an app that enables generation of PDF reports from my long days out.

- [ ] read-in and parse gps-data
    - [x] .fit [`fit`](https://github.com/kuperov/fit)
    - [ ] .gpx
    - [ ] other formats?
- [x] show some summary information about the outting
- [x] visualize the gps track using `leaflet` and `sp`
  - [ ] choose other tileservers (swisstopo?)
  - [ ] use `webshot` to download captures of the mapping
- [ ] generate figures
  - [x] speed vs. distance
  - [ ] speed vs. angle (detect noisy data points, maybe for removal?)
- [ ] allow user to enter text (title, location, description, free-text fields)
- [ ] generate pdf report
