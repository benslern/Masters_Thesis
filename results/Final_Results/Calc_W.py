# trace generated using paraview version 5.11.2
#import paraview
#paraview.compatibility.major = 5
#paraview.compatibility.minor = 11

#### import the simple module from the paraview
from paraview.simple import *
#### disable automatic camera reset on 'Show'
paraview.simple._DisableFirstRenderCameraReset()

# find source
uvec_fwdTE_0nc = GetActiveSource() #FindSource('Uvec_fwdTE_0.nc')

# create a new 'Merge Vector Components'
mergeVectorComponents1 = MergeVectorComponents(registrationName='MergeVectorComponents1', Input=uvec_fwdTE_0nc)
mergeVectorComponents1.XArray = 'Ux'
mergeVectorComponents1.YArray = 'Ux'
mergeVectorComponents1.ZArray = 'Ux'

# find source
uvec_fwdTE_visualnc = FindSource('Uvec_fwdTE_visual.nc')

# Properties modified on mergeVectorComponents1
mergeVectorComponents1.OutputVectorName = 'U'

# get active view
renderView1 = GetActiveViewOrCreate('RenderView')

# show data in view
mergeVectorComponents1Display = Show(mergeVectorComponents1, renderView1, 'UniformGridRepresentation')

# trace defaults for the display properties.
mergeVectorComponents1Display.Representation = 'Outline'
mergeVectorComponents1Display.ColorArrayName = [None, '']
mergeVectorComponents1Display.SelectTCoordArray = 'None'
mergeVectorComponents1Display.SelectNormalArray = 'None'
mergeVectorComponents1Display.SelectTangentArray = 'None'
mergeVectorComponents1Display.OSPRayScaleArray = 'U'
mergeVectorComponents1Display.OSPRayScaleFunction = 'PiecewiseFunction'
mergeVectorComponents1Display.SelectOrientationVectors = 'None'
mergeVectorComponents1Display.ScaleFactor = 12.700000000000001
mergeVectorComponents1Display.SelectScaleArray = 'None'
mergeVectorComponents1Display.GlyphType = 'Arrow'
mergeVectorComponents1Display.GlyphTableIndexArray = 'None'
mergeVectorComponents1Display.GaussianRadius = 0.635
mergeVectorComponents1Display.SetScaleArray = ['POINTS', 'U']
mergeVectorComponents1Display.ScaleTransferFunction = 'PiecewiseFunction'
mergeVectorComponents1Display.OpacityArray = ['POINTS', 'U']
mergeVectorComponents1Display.OpacityTransferFunction = 'PiecewiseFunction'
mergeVectorComponents1Display.DataAxesGrid = 'GridAxesRepresentation'
mergeVectorComponents1Display.PolarAxes = 'PolarAxesRepresentation'
mergeVectorComponents1Display.ScalarOpacityUnitDistance = 1.732050807568877
mergeVectorComponents1Display.OpacityArrayName = ['POINTS', 'U']
mergeVectorComponents1Display.ColorArray2Name = ['POINTS', 'U']
mergeVectorComponents1Display.SliceFunction = 'Plane'
mergeVectorComponents1Display.Slice = 63
mergeVectorComponents1Display.SelectInputVectors = ['POINTS', 'U']
mergeVectorComponents1Display.WriteLog = ''

# init the 'PiecewiseFunction' selected for 'ScaleTransferFunction'
mergeVectorComponents1Display.ScaleTransferFunction.Points = [-0.15915494309189532, 0.0, 0.5, 0.0, 0.15915494309189532, 1.0, 0.5, 0.0]

# init the 'PiecewiseFunction' selected for 'OpacityTransferFunction'
mergeVectorComponents1Display.OpacityTransferFunction.Points = [-0.15915494309189532, 0.0, 0.5, 0.0, 0.15915494309189532, 1.0, 0.5, 0.0]

# init the 'Plane' selected for 'SliceFunction'
mergeVectorComponents1Display.SliceFunction.Origin = [63.5, 63.5, 63.5]

# hide data in view
Hide(uvec_fwdTE_0nc, renderView1)

# update the view to ensure updated data information
renderView1.Update()

# create a new 'Python Calculator'
pythonCalculator1 = PythonCalculator(registrationName='PythonCalculator1', Input=mergeVectorComponents1)
pythonCalculator1.Expression = ''

# Properties modified on pythonCalculator1
pythonCalculator1.Expression = 'curl(U)'
pythonCalculator1.ArrayName = 'W'

# show data in view
pythonCalculator1Display = Show(pythonCalculator1, renderView1, 'UniformGridRepresentation')

# trace defaults for the display properties.
pythonCalculator1Display.Representation = 'Outline'
pythonCalculator1Display.ColorArrayName = [None, '']
pythonCalculator1Display.SelectTCoordArray = 'None'
pythonCalculator1Display.SelectNormalArray = 'None'
pythonCalculator1Display.SelectTangentArray = 'None'
pythonCalculator1Display.OSPRayScaleArray = 'U'
pythonCalculator1Display.OSPRayScaleFunction = 'PiecewiseFunction'
pythonCalculator1Display.SelectOrientationVectors = 'None'
pythonCalculator1Display.ScaleFactor = 12.700000000000001
pythonCalculator1Display.SelectScaleArray = 'None'
pythonCalculator1Display.GlyphType = 'Arrow'
pythonCalculator1Display.GlyphTableIndexArray = 'None'
pythonCalculator1Display.GaussianRadius = 0.635
pythonCalculator1Display.SetScaleArray = ['POINTS', 'U']
pythonCalculator1Display.ScaleTransferFunction = 'PiecewiseFunction'
pythonCalculator1Display.OpacityArray = ['POINTS', 'U']
pythonCalculator1Display.OpacityTransferFunction = 'PiecewiseFunction'
pythonCalculator1Display.DataAxesGrid = 'GridAxesRepresentation'
pythonCalculator1Display.PolarAxes = 'PolarAxesRepresentation'
pythonCalculator1Display.ScalarOpacityUnitDistance = 1.732050807568877
pythonCalculator1Display.OpacityArrayName = ['POINTS', 'U']
pythonCalculator1Display.ColorArray2Name = ['POINTS', 'U']
pythonCalculator1Display.SliceFunction = 'Plane'
pythonCalculator1Display.Slice = 63
pythonCalculator1Display.SelectInputVectors = ['POINTS', 'U']
pythonCalculator1Display.WriteLog = ''

# init the 'PiecewiseFunction' selected for 'ScaleTransferFunction'
pythonCalculator1Display.ScaleTransferFunction.Points = [-0.15915494309189532, 0.0, 0.5, 0.0, 0.15915494309189532, 1.0, 0.5, 0.0]

# init the 'PiecewiseFunction' selected for 'OpacityTransferFunction'
pythonCalculator1Display.OpacityTransferFunction.Points = [-0.15915494309189532, 0.0, 0.5, 0.0, 0.15915494309189532, 1.0, 0.5, 0.0]

# init the 'Plane' selected for 'SliceFunction'
pythonCalculator1Display.SliceFunction.Origin = [63.5, 63.5, 63.5]

# hide data in view
Hide(mergeVectorComponents1, renderView1)

# update the view to ensure updated data information
renderView1.Update()

#================================================================
# addendum: following script captures some of the application
# state to faithfully reproduce the visualization during playback
#================================================================

# get layout
layout1 = GetLayout()

#--------------------------------
# saving layout sizes for layouts

# layout/tab size in pixels
layout1.SetSize(1086, 703)

#-----------------------------------
# saving camera placements for views

# current camera placement for renderView1
renderView1.CameraPosition = [255.11324234404125, -288.3908273177423, 205.0625424816672]
renderView1.CameraFocalPoint = [63.5, 63.5, 63.5]
renderView1.CameraViewUp = [-0.1614563598363018, 0.2913948570683349, 0.9428790384468919]
renderView1.CameraParallelScale = 109.9852262806237

#--------------------------------------------
# uncomment the following to render all views
# RenderAllViews()
# alternatively, if you want to write images, you can use SaveScreenshot(...).