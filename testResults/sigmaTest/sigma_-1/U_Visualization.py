# trace generated using paraview version 5.11.2
#import paraview
#paraview.compatibility.major = 5
#paraview.compatibility.minor = 11

#### import the simple module from the paraview
from paraview.simple import *
#### disable automatic camera reset on 'Show'
paraview.simple._DisableFirstRenderCameraReset()

for i in range(0,300,10):
    ResetSession()

    my_iter = str(i)
    
    # create a new 'NetCDF Reader'
    uvec_fwdTE_0nc = NetCDFReader(registrationName='Uvec_fwdTE_'+my_iter+'.nc', FileName=['/home/noah/Documents/Research/Masters_Thesis/testResults/sigmaTest/sigma_-1/Uvec_fwdTE_'+my_iter+'.nc'])
    uvec_fwdTE_0nc.Dimensions = '(z, y, x)'

    # get active view
    renderView1 = GetActiveViewOrCreate('RenderView')

    # show data in view
    uvec_fwdTE_0ncDisplay = Show(uvec_fwdTE_0nc, renderView1, 'UniformGridRepresentation')

    # trace defaults for the display properties.
    uvec_fwdTE_0ncDisplay.Representation = 'Outline'
    uvec_fwdTE_0ncDisplay.ColorArrayName = [None, '']
    uvec_fwdTE_0ncDisplay.SelectTCoordArray = 'None'
    uvec_fwdTE_0ncDisplay.SelectNormalArray = 'None'
    uvec_fwdTE_0ncDisplay.SelectTangentArray = 'None'
    uvec_fwdTE_0ncDisplay.OSPRayScaleArray = 'Ux'
    uvec_fwdTE_0ncDisplay.OSPRayScaleFunction = 'PiecewiseFunction'
    uvec_fwdTE_0ncDisplay.SelectOrientationVectors = 'None'
    uvec_fwdTE_0ncDisplay.ScaleFactor = 25.5
    uvec_fwdTE_0ncDisplay.SelectScaleArray = 'None'
    uvec_fwdTE_0ncDisplay.GlyphType = 'Arrow'
    uvec_fwdTE_0ncDisplay.GlyphTableIndexArray = 'None'
    uvec_fwdTE_0ncDisplay.GaussianRadius = 1.2750000000000001
    uvec_fwdTE_0ncDisplay.SetScaleArray = ['POINTS', 'Ux']
    uvec_fwdTE_0ncDisplay.ScaleTransferFunction = 'PiecewiseFunction'
    uvec_fwdTE_0ncDisplay.OpacityArray = ['POINTS', 'Ux']
    uvec_fwdTE_0ncDisplay.OpacityTransferFunction = 'PiecewiseFunction'
    uvec_fwdTE_0ncDisplay.DataAxesGrid = 'GridAxesRepresentation'
    uvec_fwdTE_0ncDisplay.PolarAxes = 'PolarAxesRepresentation'
    uvec_fwdTE_0ncDisplay.ScalarOpacityUnitDistance = 1.7320508075688774
    uvec_fwdTE_0ncDisplay.OpacityArrayName = ['POINTS', 'Ux']
    uvec_fwdTE_0ncDisplay.ColorArray2Name = ['POINTS', 'Ux']
    uvec_fwdTE_0ncDisplay.SliceFunction = 'Plane'
    uvec_fwdTE_0ncDisplay.Slice = 127
    uvec_fwdTE_0ncDisplay.SelectInputVectors = [None, '']
    uvec_fwdTE_0ncDisplay.WriteLog = ''

    # init the 'PiecewiseFunction' selected for 'ScaleTransferFunction'
    uvec_fwdTE_0ncDisplay.ScaleTransferFunction.Points = [-0.15915494309189532, 0.0, 0.5, 0.0, 0.15915494309189532, 1.0, 0.5, 0.0]

    # init the 'PiecewiseFunction' selected for 'OpacityTransferFunction'
    uvec_fwdTE_0ncDisplay.OpacityTransferFunction.Points = [-0.15915494309189532, 0.0, 0.5, 0.0, 0.15915494309189532, 1.0, 0.5, 0.0]

    # init the 'Plane' selected for 'SliceFunction'
    uvec_fwdTE_0ncDisplay.SliceFunction.Origin = [127.5, 127.5, 127.5]

    # reset view to fit data
    renderView1.ResetCamera(False)

    # update the view to ensure updated data information
    renderView1.Update()

    # create a new 'Merge Vector Components'
    mergeVectorComponents1 = MergeVectorComponents(registrationName='MergeVectorComponents1', Input=uvec_fwdTE_0nc)
    mergeVectorComponents1.XArray = 'Ux'
    mergeVectorComponents1.YArray = 'Ux'
    mergeVectorComponents1.ZArray = 'Ux'

    # Properties modified on mergeVectorComponents1
    mergeVectorComponents1.YArray = 'Uy'
    mergeVectorComponents1.ZArray = 'Uz'
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
    mergeVectorComponents1Display.ScaleFactor = 25.5
    mergeVectorComponents1Display.SelectScaleArray = 'None'
    mergeVectorComponents1Display.GlyphType = 'Arrow'
    mergeVectorComponents1Display.GlyphTableIndexArray = 'None'
    mergeVectorComponents1Display.GaussianRadius = 1.2750000000000001
    mergeVectorComponents1Display.SetScaleArray = ['POINTS', 'U']
    mergeVectorComponents1Display.ScaleTransferFunction = 'PiecewiseFunction'
    mergeVectorComponents1Display.OpacityArray = ['POINTS', 'U']
    mergeVectorComponents1Display.OpacityTransferFunction = 'PiecewiseFunction'
    mergeVectorComponents1Display.DataAxesGrid = 'GridAxesRepresentation'
    mergeVectorComponents1Display.PolarAxes = 'PolarAxesRepresentation'
    mergeVectorComponents1Display.ScalarOpacityUnitDistance = 1.7320508075688774
    mergeVectorComponents1Display.OpacityArrayName = ['POINTS', 'U']
    mergeVectorComponents1Display.ColorArray2Name = ['POINTS', 'U']
    mergeVectorComponents1Display.SliceFunction = 'Plane'
    mergeVectorComponents1Display.Slice = 127
    mergeVectorComponents1Display.SelectInputVectors = ['POINTS', 'U']
    mergeVectorComponents1Display.WriteLog = ''

    # init the 'PiecewiseFunction' selected for 'ScaleTransferFunction'
    mergeVectorComponents1Display.ScaleTransferFunction.Points = [-0.15915494309189532, 0.0, 0.5, 0.0, 0.15915494309189532, 1.0, 0.5, 0.0]

    # init the 'PiecewiseFunction' selected for 'OpacityTransferFunction'
    mergeVectorComponents1Display.OpacityTransferFunction.Points = [-0.15915494309189532, 0.0, 0.5, 0.0, 0.15915494309189532, 1.0, 0.5, 0.0]

    # init the 'Plane' selected for 'SliceFunction'
    mergeVectorComponents1Display.SliceFunction.Origin = [127.5, 127.5, 127.5]

    # hide data in view
    Hide(uvec_fwdTE_0nc, renderView1)

    # update the view to ensure updated data information
    renderView1.Update()

    # set scalar coloring
    ColorBy(mergeVectorComponents1Display, ('POINTS', 'U', 'Magnitude'))

    # rescale color and/or opacity maps used to include current data range
    mergeVectorComponents1Display.RescaleTransferFunctionToDataRange(True, False)

    # show color bar/color legend
    mergeVectorComponents1Display.SetScalarBarVisibility(renderView1, True)

    # get color transfer function/color map for 'U'
    uLUT = GetColorTransferFunction('U')

    # get opacity transfer function/opacity map for 'U'
    uPWF = GetOpacityTransferFunction('U')

    # get 2D transfer function for 'U'
    uTF2D = GetTransferFunction2D('U')

    # change representation type
    mergeVectorComponents1Display.SetRepresentationType('Volume')

    # Rescale transfer function
    uLUT.RescaleTransferFunction(0.0, 0.34)

    # Rescale transfer function
    uPWF.RescaleTransferFunction(0.0, 0.34)

    # Rescale 2D transfer function
    uTF2D.RescaleTransferFunction(0.0, 0.34, 0.0, 1.0)

    # Properties modified on renderView1.AxesGrid
    renderView1.AxesGrid.Visibility = 1

    # Properties modified on mergeVectorComponents1Display
    mergeVectorComponents1Display.BlendMode = 'Isosurface'

    # set active source
    SetActiveSource(uvec_fwdTE_0nc)

    # toggle interactive widget visibility (only when running from the GUI)
    ShowInteractiveWidgets(proxy=uvec_fwdTE_0ncDisplay.SliceFunction)

    # toggle interactive widget visibility (only when running from the GUI)
    ShowInteractiveWidgets(proxy=uvec_fwdTE_0ncDisplay)

    # toggle interactive widget visibility (only when running from the GUI)
    HideInteractiveWidgets(proxy=uvec_fwdTE_0ncDisplay.SliceFunction)

    # toggle interactive widget visibility (only when running from the GUI)
    HideInteractiveWidgets(proxy=uvec_fwdTE_0ncDisplay)

    # set active source
    SetActiveSource(mergeVectorComponents1)

    # toggle interactive widget visibility (only when running from the GUI)
    ShowInteractiveWidgets(proxy=mergeVectorComponents1Display.SliceFunction)

    # toggle interactive widget visibility (only when running from the GUI)
    ShowInteractiveWidgets(proxy=mergeVectorComponents1Display)

    # toggle interactive widget visibility (only when running from the GUI)
    HideInteractiveWidgets(proxy=mergeVectorComponents1Display.SliceFunction)

    # toggle interactive widget visibility (only when running from the GUI)
    HideInteractiveWidgets(proxy=mergeVectorComponents1Display)

    # Properties modified on mergeVectorComponents1Display
    mergeVectorComponents1Display.IsosurfaceValues = [0.0, 0.03777777777777778, 0.07555555555555556, 0.11333333333333334, 0.1511111111111111, 0.18888888888888888, 0.22666666666666668, 0.2644444444444444, 0.3022222222222222, 0.34]

    renderView1.ApplyIsometricView()

    # reset view to fit data
    renderView1.ResetCamera(False)

    # get layout
    layout1 = GetLayout()

    # layout/tab size in pixels
    layout1.SetSize(1366, 703)

    # current camera placement for renderView1
    renderView1.CameraPosition = [785.410448425272, 620.122171407425, 356.6441672336226]
    renderView1.CameraFocalPoint = [127.49999999999996, 127.49999999999996, 127.49999999999996]
    renderView1.CameraViewUp = [-0.5452268116162228, 0.816496580927726, -0.1898974913687419]
    renderView1.CameraParallelScale = 220.83647796503186

    # save screenshot
    SaveScreenshot('/home/noah/Documents/Research/Masters_Thesis/testResults/sigmaTest/sigma_-1/U_iter_'+my_iter+'.png', renderView1, ImageResolution=[1366, 703])

    #================================================================
    # addendum: following script captures some of the application
    # state to faithfully reproduce the visualization during playback
    #================================================================

    #--------------------------------
    # saving layout sizes for layouts

    # layout/tab size in pixels
    layout1.SetSize(1366, 703)

    #-----------------------------------
    # saving camera placements for views

    # current camera placement for renderView1
    renderView1.CameraPosition = [785.410448425272, 620.122171407425, 356.6441672336226]
    renderView1.CameraFocalPoint = [127.49999999999996, 127.49999999999996, 127.49999999999996]
    renderView1.CameraViewUp = [-0.5452268116162228, 0.816496580927726, -0.1898974913687419]
    renderView1.CameraParallelScale = 220.83647796503186

    #--------------------------------------------
    # uncomment the following to render all views
    # RenderAllViews()
    # alternatively, if you want to write images, you can use SaveScreenshot(...).
