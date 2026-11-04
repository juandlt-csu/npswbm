# npswbm

An R implementation of the National Park Service (NPS) water balance model for
point locations, with runoff routing, model-fit metrics, and helpers that
retrieve the gridded water balance model data it uses. Part of Water
Vulnerability Analytics (WaVA); the deep-dive assessments use it. Every
function has a help page, e.g. `?nps_wbm`.

## What is in it

| Function family | What it does |
| --- | --- |
| `nps_wbm()`, `run_nps_wbm_points()` | Run the model for one site, or for many points and climate models |
| `load_wbm_rasters()`, `get_nps_wbm_params()` | Site parameters (elevation, slope, aspect, soil water capacity, rain-snow temperature) |
| `get_freeze()`, `get_rain()`, `get_snow()`, `get_melt()`, `get_melt_ros()`, `get_snowpack()`, `get_soil()`, `get_aet()`, `get_storage()`, `get_gdd()`, `get_daylength()` | The model's daily steps |
| `get_oudin_pet()`, `et_hamon_daily()`, `et_penman_monteith_daily()` | Potential evapotranspiration methods |
| `runoff_routing_gw()`, `runoff_routing_snow_fun()`, `test_model_fit()` | Route model runoff to streamflow and fit it to a gage |
| `nse()`, `rsr()`, `pbias()`, `cod()`, `sse()`, `rmse()`, `mae()` | Model fit metrics |
| `get_wbm_grids()`, `get_wbm_aoi()`, `get_future_wbm()`, `get_30_year_wbm_gridmet()`, `get_historic_wbm_daymet()` | Download gridded NPS water balance model data |

## Install

`npswbm` depends on [`npswavacore`](https://github.com/Water-Vulnerability-Analytics/npswavacore).

```r
# install.packages("remotes")
remotes::install_github("Water-Vulnerability-Analytics/npswbm")
```

## Data

The model reads gridded inputs from the shared WaVA data folder, which is not
part of this repository. See `npswavacore::wava_data_path()` for how that folder
is found.

## AI Use Disclosure

AI-assisted coding tools (Claude Code, using Anthropic's Claude models) were used
during development of this repository, beginning in 2026, to help set up the
package, prepare it for release, and write tests. All AI-assisted code and text
was reviewed, tested, and validated by the authors, who made all design and
scientific decisions and remain responsible for the content.

## License

MIT. See [LICENSE.md](LICENSE.md).
