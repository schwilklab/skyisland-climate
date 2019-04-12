# Notes on modeling photosynthesis in sky islands dataset

## Necessary data
In order to run the optimal photosynthesis model, we need the following:
- Temperature
- VPD
- Elevation
- Light availability (PAR)
- CO2

### Temperature
- This can be taken directly from the [iButton](../microclimate/raw-ibutton/) data (I think)
- It looks like the tricky part will be piecing these data together

### VPD
- Humidity can be taken directly from the [iButton](../microclimate/raw-ibutton/) data (I think)
- Will then need to combine this with temperature
	- See here: [http://cronklab.wikidot.com/calculation-of-vapour-pressure-deficit](http://cronklab.wikidot.com/calculation-of-vapour-pressure-deficit)
- It looks like the tricky part will be piecing these data together
