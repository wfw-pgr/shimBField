import numpy                  as np
import myStyle.LoadConfig     as lcf
import myStyle.configSettings as cfs
import myStyle.cMap2D         as cm2
import myStyle.cMapTri        as cmt

# ========================================================= #
# ===  show shim bfield                                 === #
# ========================================================= #
def show__bfield( datFile=None, pngFile=None, config=None ):
    # ------------------------------------------------- #
    # --- [1] Arguments                             --- #
    # ------------------------------------------------- #
    if ( datFile is None ): datFile = "dat/shimField.dat"
    if ( pngFile is None ): pngFile = "png/shimField.png"
    if ( config  is None ): config  = lcf.LoadConfig()

    # ------------------------------------------------- #
    # --- [2] Fetch Data                            --- #
    # ------------------------------------------------- #
    with open( datFile, "r" ) as f:
        Data = np.loadtxt( f )
        
    # ------------------------------------------------- #
    # --- [3] config Settings                       --- #
    # ------------------------------------------------- #
    cfs.configSettings( configType="cMap2D_def", config=config )
    config["xTitle"]         = "$X$" + " [m]"
    config["yTitle"]         = "$Y$" + " [m]"
    config["cmp_AutoLevel"]  = True
    config["cmp_MaxMin"]     = [0.0,+1.0]
    config["cmp_xAutoRange"] = True
    config["cmp_yAutoRange"] = True
    config["cmp_xRange"]     = [-5.0,+5.0]
    config["cmp_yRange"]     = [-5.0,+5.0]
    config["xMajor_Nticks"]  = 5
    config["yMajor_Nticks"]  = 5
    config["MinimalOut"]     = False

    # ------------------------------------------------- #
    # --- [4] plot Figure                           --- #
    # ------------------------------------------------- #
    fig = cmt.cMapTri( xAxis=xAxis, yAxis=yAxis, cMap=cMap, FigName=pngFile, config=config )
    fig = cm2.cMap2D ( xAxis=xAxis, yAxis=yAxis, cMap=cMap, FigName=pngFile, config=config )


# ======================================== #
# ===  実行部                          === #
# ======================================== #
if ( __name__=="__main__" ):
    import myUtils.genArgs as gar
    args    = gar.genArgs()
    show__bfield()
    
