# trace generated using paraview version 6.0.0
#import paraview
#paraview.compatibility.major = 6
#paraview.compatibility.minor = 0

#### import the simple module from the paraview
from paraview.simple import *

# find source
activeSource = GetActiveSource()

# create a new 'Calculator'
calculator1 = Calculator(registrationName='w_i^2', Input=activeSource)

# Properties modified on calculator1
calculator1.Set(
    ResultArrayName='w_i^2',
    Function='Vorticity_X*Vorticity_X*iHat+Vorticity_Y*Vorticity_Y*jHat+Vorticity_Z*Vorticity_Z*kHat',
)

# create a new 'Calculator'
calculator2 = Calculator(registrationName='u_i^2', Input=calculator1)

# set active source
SetActiveSource(calculator2)

# Properties modified on calculator2
calculator2.Set(
    ResultArrayName='u_i^2',
    Function='u_X*u_X*iHat+u_Y*u_Y*jHat+u_Z*u_Z*kHat',
)


# create a new 'Integrate Variables'
integrateVariables1 = IntegrateVariables(registrationName='IntegrateVariables1', Input=calculator2)

# get active view
spreadSheetView1 = GetActiveViewOrCreate('SpreadSheetView')

# show data in view
integrateVariables1Display = Show(integrateVariables1, spreadSheetView1, 'SpreadSheetRepresentation')

# find source
w_i2 = FindSource('w_i^2')

# update the view to ensure updated data information
spreadSheetView1.Update()

#================================================================
# addendum: following script captures some of the application
# state to faithfully reproduce the visualization during playback
#================================================================

# get layout
layout1 = GetLayout()

#--------------------------------
# saving layout sizes for layouts

# layout/tab size in pixels
layout1.SetSize(1176, 811)
