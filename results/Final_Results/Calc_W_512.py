# trace generated using paraview version 5.11.2
#import paraview
#paraview.compatibility.major = 5
#paraview.compatibility.minor = 11

#### import the simple module from the paraview
from paraview.simple import *
#### disable automatic camera reset on 'Show'
paraview.simple._DisableFirstRenderCameraReset()

# find source
uvec_fwdTE_20nc = GetActiveSource() # FindSource('Uvec_fwdTE_20.nc')

# create a new 'Transform'
transform1 = Transform(registrationName='Transform1', Input=uvec_fwdTE_20nc)
transform1.Transform = 'Transform'

# find source
uvec_fwdTE_visualnc = FindSource('Uvec_fwdTE_visual.nc')

# Properties modified on transform1.Transform
transform1.Transform.Scale = [2.001956947, 2.001956947, 2.001956947]

# get active view
renderView1 = GetActiveViewOrCreate('RenderView')

# show data in view
transform1Display = Show(transform1, renderView1, 'StructuredGridRepresentation')

# trace defaults for the display properties.
transform1Display.Representation = 'Outline'
transform1Display.ColorArrayName = [None, '']
transform1Display.SelectTCoordArray = 'None'
transform1Display.SelectNormalArray = 'None'
transform1Display.SelectTangentArray = 'None'
transform1Display.OSPRayScaleArray = 'Ux'
transform1Display.OSPRayScaleFunction = 'PiecewiseFunction'
transform1Display.SelectOrientationVectors = 'None'
transform1Display.ScaleFactor = 102.2999999917
transform1Display.SelectScaleArray = 'None'
transform1Display.GlyphType = 'Arrow'
transform1Display.GlyphTableIndexArray = 'None'
transform1Display.GaussianRadius = 5.114999999585
transform1Display.SetScaleArray = ['POINTS', 'Ux']
transform1Display.ScaleTransferFunction = 'PiecewiseFunction'
transform1Display.OpacityArray = ['POINTS', 'Ux']
transform1Display.OpacityTransferFunction = 'PiecewiseFunction'
transform1Display.DataAxesGrid = 'GridAxesRepresentation'
transform1Display.PolarAxes = 'PolarAxesRepresentation'
transform1Display.ScalarOpacityUnitDistance = 3.467491146769474
transform1Display.SelectInputVectors = [None, '']
transform1Display.WriteLog = ''

# init the 'PiecewiseFunction' selected for 'ScaleTransferFunction'
transform1Display.ScaleTransferFunction.Points = [-0.22690852557249633, 0.0, 0.5, 0.0, 0.22680368696138095, 1.0, 0.5, 0.0]

# init the 'PiecewiseFunction' selected for 'OpacityTransferFunction'
transform1Display.OpacityTransferFunction.Points = [-0.22690852557249633, 0.0, 0.5, 0.0, 0.22680368696138095, 1.0, 0.5, 0.0]

# hide data in view
Hide(uvec_fwdTE_20nc, renderView1)

# update the view to ensure updated data information
renderView1.Update()

# create a new 'Resample To Image'
resampleToImage1 = ResampleToImage(registrationName='ResampleToImage1', Input=transform1)
resampleToImage1.SamplingBounds = [0.0, 1022.999999917, 0.0, 1022.999999917, 0.0, 1022.999999917]

# Properties modified on resampleToImage1
resampleToImage1.UseInputBounds = 0
resampleToImage1.SamplingDimensions = [512, 512, 512]

# show data in view
resampleToImage1Display = Show(resampleToImage1, renderView1, 'UniformGridRepresentation')

# trace defaults for the display properties.
resampleToImage1Display.Representation = 'Outline'
resampleToImage1Display.ColorArrayName = [None, '']
resampleToImage1Display.SelectTCoordArray = 'None'
resampleToImage1Display.SelectNormalArray = 'None'
resampleToImage1Display.SelectTangentArray = 'None'
resampleToImage1Display.OSPRayScaleArray = 'Ux'
resampleToImage1Display.OSPRayScaleFunction = 'PiecewiseFunction'
resampleToImage1Display.SelectOrientationVectors = 'None'
resampleToImage1Display.ScaleFactor = 102.2999999917
resampleToImage1Display.SelectScaleArray = 'None'
resampleToImage1Display.GlyphType = 'Arrow'
resampleToImage1Display.GlyphTableIndexArray = 'None'
resampleToImage1Display.GaussianRadius = 5.114999999585
resampleToImage1Display.SetScaleArray = ['POINTS', 'Ux']
resampleToImage1Display.ScaleTransferFunction = 'PiecewiseFunction'
resampleToImage1Display.OpacityArray = ['POINTS', 'Ux']
resampleToImage1Display.OpacityTransferFunction = 'PiecewiseFunction'
resampleToImage1Display.DataAxesGrid = 'GridAxesRepresentation'
resampleToImage1Display.PolarAxes = 'PolarAxesRepresentation'
resampleToImage1Display.ScalarOpacityUnitDistance = 3.467491146769474
resampleToImage1Display.OpacityArrayName = ['POINTS', 'Ux']
resampleToImage1Display.ColorArray2Name = ['POINTS', 'Ux']
resampleToImage1Display.SliceFunction = 'Plane'
resampleToImage1Display.Slice = 255
resampleToImage1Display.SelectInputVectors = [None, '']
resampleToImage1Display.WriteLog = ''

# init the 'PiecewiseFunction' selected for 'ScaleTransferFunction'
resampleToImage1Display.ScaleTransferFunction.Points = [-0.22690852557249633, 0.0, 0.5, 0.0, 0.22680368696138095, 1.0, 0.5, 0.0]

# init the 'PiecewiseFunction' selected for 'OpacityTransferFunction'
resampleToImage1Display.OpacityTransferFunction.Points = [-0.22690852557249633, 0.0, 0.5, 0.0, 0.22680368696138095, 1.0, 0.5, 0.0]

# init the 'Plane' selected for 'SliceFunction'
resampleToImage1Display.SliceFunction.Origin = [511.4999999585, 511.4999999585, 511.4999999585]

# hide data in view
Hide(transform1, renderView1)

# update the view to ensure updated data information
renderView1.Update()

# set active source
SetActiveSource(transform1)

# toggle interactive widget visibility (only when running from the GUI)
ShowInteractiveWidgets(proxy=transform1.Transform)

# set active source
SetActiveSource(resampleToImage1)

# toggle interactive widget visibility (only when running from the GUI)
ShowInteractiveWidgets(proxy=resampleToImage1Display.SliceFunction)

# toggle interactive widget visibility (only when running from the GUI)
ShowInteractiveWidgets(proxy=resampleToImage1Display)

# toggle interactive widget visibility (only when running from the GUI)
HideInteractiveWidgets(proxy=resampleToImage1Display.SliceFunction)

# toggle interactive widget visibility (only when running from the GUI)
HideInteractiveWidgets(proxy=resampleToImage1Display)

# toggle interactive widget visibility (only when running from the GUI)
HideInteractiveWidgets(proxy=transform1.Transform)

# create a new 'Merge Vector Components'
mergeVectorComponents1 = MergeVectorComponents(registrationName='MergeVectorComponents1', Input=resampleToImage1)
mergeVectorComponents1.XArray = 'Ux'
mergeVectorComponents1.YArray = 'Ux'
mergeVectorComponents1.ZArray = 'Ux'

# Properties modified on mergeVectorComponents1
mergeVectorComponents1.OutputVectorName = 'U'

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
mergeVectorComponents1Display.ScaleFactor = 102.2999999917
mergeVectorComponents1Display.SelectScaleArray = 'None'
mergeVectorComponents1Display.GlyphType = 'Arrow'
mergeVectorComponents1Display.GlyphTableIndexArray = 'None'
mergeVectorComponents1Display.GaussianRadius = 5.114999999585
mergeVectorComponents1Display.SetScaleArray = ['POINTS', 'U']
mergeVectorComponents1Display.ScaleTransferFunction = 'PiecewiseFunction'
mergeVectorComponents1Display.OpacityArray = ['POINTS', 'U']
mergeVectorComponents1Display.OpacityTransferFunction = 'PiecewiseFunction'
mergeVectorComponents1Display.DataAxesGrid = 'GridAxesRepresentation'
mergeVectorComponents1Display.PolarAxes = 'PolarAxesRepresentation'
mergeVectorComponents1Display.ScalarOpacityUnitDistance = 3.467491146769474
mergeVectorComponents1Display.OpacityArrayName = ['POINTS', 'U']
mergeVectorComponents1Display.ColorArray2Name = ['POINTS', 'U']
mergeVectorComponents1Display.SliceFunction = 'Plane'
mergeVectorComponents1Display.Slice = 255
mergeVectorComponents1Display.SelectInputVectors = ['POINTS', 'U']
mergeVectorComponents1Display.WriteLog = ''

# init the 'PiecewiseFunction' selected for 'ScaleTransferFunction'
mergeVectorComponents1Display.ScaleTransferFunction.Points = [-0.22690852557249633, 0.0, 0.5, 0.0, 0.22680368696138095, 1.0, 0.5, 0.0]

# init the 'PiecewiseFunction' selected for 'OpacityTransferFunction'
mergeVectorComponents1Display.OpacityTransferFunction.Points = [-0.22690852557249633, 0.0, 0.5, 0.0, 0.22680368696138095, 1.0, 0.5, 0.0]

# init the 'Plane' selected for 'SliceFunction'
mergeVectorComponents1Display.SliceFunction.Origin = [511.4999999585, 511.4999999585, 511.4999999585]

# hide data in view
Hide(resampleToImage1, renderView1)

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
pythonCalculator1Display.ScaleFactor = 102.2999999917
pythonCalculator1Display.SelectScaleArray = 'None'
pythonCalculator1Display.GlyphType = 'Arrow'
pythonCalculator1Display.GlyphTableIndexArray = 'None'
pythonCalculator1Display.GaussianRadius = 5.114999999585
pythonCalculator1Display.SetScaleArray = ['POINTS', 'U']
pythonCalculator1Display.ScaleTransferFunction = 'PiecewiseFunction'
pythonCalculator1Display.OpacityArray = ['POINTS', 'U']
pythonCalculator1Display.OpacityTransferFunction = 'PiecewiseFunction'
pythonCalculator1Display.DataAxesGrid = 'GridAxesRepresentation'
pythonCalculator1Display.PolarAxes = 'PolarAxesRepresentation'
pythonCalculator1Display.ScalarOpacityUnitDistance = 3.467491146769474
pythonCalculator1Display.OpacityArrayName = ['POINTS', 'U']
pythonCalculator1Display.ColorArray2Name = ['POINTS', 'U']
pythonCalculator1Display.SliceFunction = 'Plane'
pythonCalculator1Display.Slice = 255
pythonCalculator1Display.SelectInputVectors = ['POINTS', 'U']
pythonCalculator1Display.WriteLog = ''

# init the 'PiecewiseFunction' selected for 'ScaleTransferFunction'
pythonCalculator1Display.ScaleTransferFunction.Points = [-0.22690852557249633, 0.0, 0.5, 0.0, 0.22680368696138095, 1.0, 0.5, 0.0]

# init the 'PiecewiseFunction' selected for 'OpacityTransferFunction'
pythonCalculator1Display.OpacityTransferFunction.Points = [-0.22690852557249633, 0.0, 0.5, 0.0, 0.22680368696138095, 1.0, 0.5, 0.0]

# init the 'Plane' selected for 'SliceFunction'
pythonCalculator1Display.SliceFunction.Origin = [511.4999999585, 511.4999999585, 511.4999999585]

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
layout1.SetSize(1043, 703)

#-----------------------------------
# saving camera placements for views

# current camera placement for renderView1
renderView1.CameraPosition = [1639.6295782776215, 1071.0235899441977, 840.8692010612789]
renderView1.CameraFocalPoint = [255.5, 255.5, 255.5]
renderView1.CameraViewUp = [-0.28971416830245217, -0.1826616985064116, 0.9395213699451287]
renderView1.CameraParallelScale = 442.53898133384814

#--------------------------------------------
# uncomment the following to render all views
# RenderAllViews()
# alternatively, if you want to write images, you can use SaveScreenshot(...).