# trace generated using paraview version 5.11.2
#import paraview
#paraview.compatibility.major = 5
#paraview.compatibility.minor = 11

#### import the simple module from the paraview
from paraview.simple import *
#### disable automatic camera reset on 'Show'
paraview.simple._DisableFirstRenderCameraReset()

# find source
uvec_fwdTE_3nc = FindSource('Uvec_fwdTE_3.nc')

# create a new 'Transform'
transform1 = Transform(registrationName='Transform1', Input=uvec_fwdTE_3nc)
transform1.Transform = 'Transform'

# Properties modified on transform1.Transform
transform1.Transform.Scale = [4.0117647059, 4.0117647059, 4.0117647059]

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
transform1Display.ScaleFactor = 102.30000000045001
transform1Display.SelectScaleArray = 'None'
transform1Display.GlyphType = 'Arrow'
transform1Display.GlyphTableIndexArray = 'None'
transform1Display.GaussianRadius = 5.1150000000225
transform1Display.SetScaleArray = ['POINTS', 'Ux']
transform1Display.ScaleTransferFunction = 'PiecewiseFunction'
transform1Display.OpacityArray = ['POINTS', 'Ux']
transform1Display.OpacityTransferFunction = 'PiecewiseFunction'
transform1Display.DataAxesGrid = 'GridAxesRepresentation'
transform1Display.PolarAxes = 'PolarAxesRepresentation'
transform1Display.ScalarOpacityUnitDistance = 6.948580298630414
transform1Display.SelectInputVectors = [None, '']
transform1Display.WriteLog = ''

# init the 'PiecewiseFunction' selected for 'ScaleTransferFunction'
transform1Display.ScaleTransferFunction.Points = [-0.23645098052322233, 0.0, 0.5, 0.0, 0.23647311906512614, 1.0, 0.5, 0.0]

# init the 'PiecewiseFunction' selected for 'OpacityTransferFunction'
transform1Display.OpacityTransferFunction.Points = [-0.23645098052322233, 0.0, 0.5, 0.0, 0.23647311906512614, 1.0, 0.5, 0.0]

# hide data in view
Hide(uvec_fwdTE_3nc, renderView1)

# update the view to ensure updated data information
renderView1.Update()

# create a new 'Merge Vector Components'
mergeVectorComponents1 = MergeVectorComponents(registrationName='MergeVectorComponents1', Input=transform1)
mergeVectorComponents1.XArray = 'Ux'
mergeVectorComponents1.YArray = 'Ux'
mergeVectorComponents1.ZArray = 'Ux'

# Properties modified on mergeVectorComponents1
mergeVectorComponents1.OutputVectorName = 'U'

# show data in view
mergeVectorComponents1Display = Show(mergeVectorComponents1, renderView1, 'StructuredGridRepresentation')

# trace defaults for the display properties.
mergeVectorComponents1Display.Representation = 'Outline'
mergeVectorComponents1Display.ColorArrayName = [None, '']
mergeVectorComponents1Display.SelectTCoordArray = 'None'
mergeVectorComponents1Display.SelectNormalArray = 'None'
mergeVectorComponents1Display.SelectTangentArray = 'None'
mergeVectorComponents1Display.OSPRayScaleArray = 'U'
mergeVectorComponents1Display.OSPRayScaleFunction = 'PiecewiseFunction'
mergeVectorComponents1Display.SelectOrientationVectors = 'None'
mergeVectorComponents1Display.ScaleFactor = 102.30000000045001
mergeVectorComponents1Display.SelectScaleArray = 'None'
mergeVectorComponents1Display.GlyphType = 'Arrow'
mergeVectorComponents1Display.GlyphTableIndexArray = 'None'
mergeVectorComponents1Display.GaussianRadius = 5.1150000000225
mergeVectorComponents1Display.SetScaleArray = ['POINTS', 'U']
mergeVectorComponents1Display.ScaleTransferFunction = 'PiecewiseFunction'
mergeVectorComponents1Display.OpacityArray = ['POINTS', 'U']
mergeVectorComponents1Display.OpacityTransferFunction = 'PiecewiseFunction'
mergeVectorComponents1Display.DataAxesGrid = 'GridAxesRepresentation'
mergeVectorComponents1Display.PolarAxes = 'PolarAxesRepresentation'
mergeVectorComponents1Display.ScalarOpacityUnitDistance = 6.948580298630414
mergeVectorComponents1Display.SelectInputVectors = ['POINTS', 'U']
mergeVectorComponents1Display.WriteLog = ''

# init the 'PiecewiseFunction' selected for 'ScaleTransferFunction'
mergeVectorComponents1Display.ScaleTransferFunction.Points = [-0.23645098052322233, 0.0, 0.5, 0.0, 0.23647311906512614, 1.0, 0.5, 0.0]

# init the 'PiecewiseFunction' selected for 'OpacityTransferFunction'
mergeVectorComponents1Display.OpacityTransferFunction.Points = [-0.23645098052322233, 0.0, 0.5, 0.0, 0.23647311906512614, 1.0, 0.5, 0.0]

# hide data in view
Hide(transform1, renderView1)

# update the view to ensure updated data information
renderView1.Update()

# create a new 'Python Calculator'
pythonCalculator1 = PythonCalculator(registrationName='PythonCalculator1', Input=mergeVectorComponents1)
pythonCalculator1.Expression = ''

# Properties modified on pythonCalculator1
pythonCalculator1.Expression = 'curl(U)'
pythonCalculator1.ArrayName = 'W'

# show data in view
pythonCalculator1Display = Show(pythonCalculator1, renderView1, 'StructuredGridRepresentation')

# trace defaults for the display properties.
pythonCalculator1Display.Representation = 'Outline'
pythonCalculator1Display.ColorArrayName = [None, '']
pythonCalculator1Display.SelectTCoordArray = 'None'
pythonCalculator1Display.SelectNormalArray = 'None'
pythonCalculator1Display.SelectTangentArray = 'None'
pythonCalculator1Display.OSPRayScaleArray = 'U'
pythonCalculator1Display.OSPRayScaleFunction = 'PiecewiseFunction'
pythonCalculator1Display.SelectOrientationVectors = 'None'
pythonCalculator1Display.ScaleFactor = 102.30000000045001
pythonCalculator1Display.SelectScaleArray = 'None'
pythonCalculator1Display.GlyphType = 'Arrow'
pythonCalculator1Display.GlyphTableIndexArray = 'None'
pythonCalculator1Display.GaussianRadius = 5.1150000000225
pythonCalculator1Display.SetScaleArray = ['POINTS', 'U']
pythonCalculator1Display.ScaleTransferFunction = 'PiecewiseFunction'
pythonCalculator1Display.OpacityArray = ['POINTS', 'U']
pythonCalculator1Display.OpacityTransferFunction = 'PiecewiseFunction'
pythonCalculator1Display.DataAxesGrid = 'GridAxesRepresentation'
pythonCalculator1Display.PolarAxes = 'PolarAxesRepresentation'
pythonCalculator1Display.ScalarOpacityUnitDistance = 6.948580298630414
pythonCalculator1Display.SelectInputVectors = ['POINTS', 'U']
pythonCalculator1Display.WriteLog = ''

# init the 'PiecewiseFunction' selected for 'ScaleTransferFunction'
pythonCalculator1Display.ScaleTransferFunction.Points = [-0.23645098052322233, 0.0, 0.5, 0.0, 0.23647311906512614, 1.0, 0.5, 0.0]

# init the 'PiecewiseFunction' selected for 'OpacityTransferFunction'
pythonCalculator1Display.OpacityTransferFunction.Points = [-0.23645098052322233, 0.0, 0.5, 0.0, 0.23647311906512614, 1.0, 0.5, 0.0]

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
renderView1.CameraPosition = [116.33965448054252, 127.17291031433425, 1376.6884951921286]
renderView1.CameraFocalPoint = [127.5, 127.5, 127.5]
renderView1.CameraViewUp = [0.9995308674657101, 0.029294454789286706, 0.008937555697366553]
renderView1.CameraParallelScale = 220.83647796503186

#--------------------------------------------
# uncomment the following to render all views
# RenderAllViews()
# alternatively, if you want to write images, you can use SaveScreenshot(...).