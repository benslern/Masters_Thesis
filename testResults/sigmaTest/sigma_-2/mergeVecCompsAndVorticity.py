
# trace generated using paraview version 5.13.1
#import paraview
#paraview.compatibility.major = 5
#paraview.compatibility.minor = 13

#### import the simple module from the paraview
from paraview.simple import *

### merge components ###
# get active source.
activeSource = GetActiveSource()

# create a new 'Merge Vector Components'
mergeVectorComponents1 = MergeVectorComponents(registrationName='u', Input=activeSource)

# Properties modified on mergeVectorComponents1
mergeVectorComponents1.XArray = 'Ux'
mergeVectorComponents1.YArray = 'Uy'
mergeVectorComponents1.ZArray = 'Uz'
mergeVectorComponents1.OutputVectorName = 'u'

# get active view
activeView = GetActiveViewOrCreate('RenderView')

# show data in view
uDisplay = Show(mergeVectorComponents1, activeView, 'UniformGridRepresentation')


### calc vortictiy ###
# get active source.
activeSource = GetActiveSource()

# create a new 'Gradient'
gradient = Gradient(registrationName='u_w', Input=activeSource)

# Properties modified on gradient1
gradient.ScalarArray = ['POINTS', 'u']
gradient.ComputeGradient = 0
gradient.ComputeVorticity = 1

# get active view
activeView = GetActiveViewOrCreate('RenderView')

# show data in view
gradientDisplay = Show(gradient, activeView, 'UniformGridRepresentation')