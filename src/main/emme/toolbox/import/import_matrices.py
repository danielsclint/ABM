#//////////////////////////////////////////////////////////////////////////////
#////                                                                       ///
#//// Copyright INRO, 2016-2017.                                            ///
#//// Rights to use and modify are granted to the                           ///
#//// San Diego Association of Governments and partner agencies.            ///
#//// This copyright notice must be preserved.                              ///
#////                                                                       ///
#//// import/import_matrices.py                                             ///
#////                                                                       ///
#////                                                                       ///
#////                                                                       ///
#////                                                                       ///
#//////////////////////////////////////////////////////////////////////////////

TOOLBOX_ORDER = 11


import inro.modeller as _m
import traceback as _traceback


class ImportMatrices(_m.Tool()):

    omx_file = _m.Attribute(unicode)
    demand_type = _m.Attribute(str)
    period = _m.Attribute(str)
    num_processors = _m.Attribute(str)
    tool_run_msg = ""

    @_m.method(return_type=_m.UnicodeType)
    def tool_run_msg_status(self):
        return self.tool_run_msg

    def __init__(self):
        project_dir = os.path.dirname(_m.Modeller().desktop.project.path)
        self.input_directory = os.path.join(os.path.dirname(project_dir), "input")
        self.convert_truck_to_pce = True
        self.num_processors = "MAX-1"
        
    def page(self):
        pb = _m.ToolPageBuilder(self)
        pb.title = "Import demand matrices"
        pb.description = """."""
        pb.branding_text = "- SANDAG - Import"
        if self.tool_run_msg != "":
            pb.tool_run_status(self.tool_run_msg_status)

        pb.add_select_file('omx_file', 'file', title='Select input OMX file')
        
        options = [(x, x) for x in ["EA", "AM", "MD", "PM", "EV"]]
        pb.add_select("period", keyvalues=options, title="Select corresponding period")
        
        options = [(x, x) for x in ["AUTO", "TRUCK", "TRANSIT"]]
        pb.add_select("demand_type", keyvalues=options, title="Select corresponding demand type")
        
        pb.add_checkbox('convert_truck_to_pce', title = " ", label="Convert demand matrices to PCE",
            note="Note: assignment demand matrices must be in PCE")
            
        dem_utils.add_select_processors("num_processors", pb, self)
        return pb.render()

    def run(self):
        self.tool_run_msg = ""
        try:
            scenario = _m.Modeller().scenario
            self(self.omx_file, self.demand_type, self.period, 
                 scenario, self.num_processors, self.convert_truck_to_pce)
            run_msg = "Tool completed"
            self.tool_run_msg = _m.PageBuilder.format_info(run_msg)
        except Exception as error:
            self.tool_run_msg = _m.PageBuilder.format_exception(
                error, _traceback.format_exc(error))
            raise

    def __call__(self, omx_file, demand_type, period, scenario,
                 num_processors=None, convert_truck_to_pce=None):
        with _m.logbook_trace("Import %s matrices for period %s" % (demand_type, period)):
            demand_types = ["AUTO", "TRUCK", "TRANSIT"]
            if demand_type not in demand_types:
                raise Exception("Invalid demand_type, must be one of %s" % demand_types)
            periods = ["EA", "AM", "MD", "PM", "EV"]
            if period not in periods:
                raise Exception("Invalid period, must be one of %s" % periods)

            import_from_omx = _m.Modeller().tool(
                "inro.emme.data.matrix.import_from_omx")
            if demand_type == "AUTO":
                matrices = {
                    'SOV_GP':   'mf"%s_SOVGP"',
                    'SOV_PAY':  'mf"%s_SOVTOLL"',
                    'SR2_GP':   'mf"%s_HOV2GP"',
                    'SR2_HOV':  'mf"%s_HOV2HOV"',
                    'SR2_PAY':  'mf"%s_HOV2TOLL"',
                    'SR3_GP':   'mf"%s_HOV3GP"',
                    'SR3_HOV':  'mf"%s_HOV3HOV"',
                    'SR3_PAY':  'mf"%s_HOV3TOLL"'}
            if demand_type == "TRUCK":
                matrices = {
                    'hhdn':     'mf"%s_TRKHGP"',
                    'hhdt':     'mf"%s_TRKHTOLL"',
                    'lhdn':     'mf"%s_TRKLGP"',
                    'lhdt':     'mf"%s_TRKLTOLL"',
                    'mhdn':     'mf"%s_TRKMGP"',
                    'mhdt':     'mf"%s_TRKMTOLL"'}
            if demand_type == "TRANSIT":
                matrices = {
                    'WLK_LOC':  'mf"%s_WLKBUS"',
                    'WLK_LRT':  'mf"%s_WLKLRT"',
                    'WLK_CMR':  'mf"%s_WLKCMR"',
                    'WLK_EXP':  'mf"%s_WLKEXP"',
                    'WLK_BRT':  'mf"%s_WLKBRT"',
                    'PNR_LOC':  'mf"%s_PNRBUS"',
                    'PNR_LRT':  'mf"%s_PNRLRT"',
                    'PNR_CMR':  'mf"%s_PNRCMR"',
                    'PNR_EXP':  'mf"%s_PNREXP"',
                    'PNR_BRT':  'mf"%s_PNRBRT"',
                    'KNR_LOC':  'mf"%s_KNRBUS"',
                    'KNR_LRT':  'mf"%s_KNRLRT"',
                    'KNR_CMR':  'mf"%s_KNRCMR"',
                    'KNR_EXP':  'mf"%s_KNREXP"',
                    'KNR_BRT':  'mf"%s_KNRBRT"'}
            matrices = dict((k, v % period) for k, v in matrices.iteritems())
            import_from_omx(file_path=omx_file, matrices=matrices, scenario=scenario)
            
            if demand_type == "TRUCK" and convert_truck_to_pce:
                self.convert_to_pce(scenario, period, num_processors)

    @_m.logbook_trace('Convert truck vehicle demand to PCE')
    def convert_to_pce(self, scenario, period, num_processors):
        matrix_calc = dem_utils.MatrixCalculator(scenario, num_processors)
        # Calculate PCEs for trucks
        mat_trucks = ['TRKHGP',   'TRKHTOLL', 'TRKLGP',   'TRKLTOLL', 'TRKMGP',   'TRKMTOLL']
        pce_values = [2.5,        2.5,        1.3,        1.3,        1.5,        1.5]
        for name, pce in zip(mat_trucks, pce_values):
            matrix_calc.add(demand_name, '(%s * %s).max.0' % (demand_name, pce))
        matrix_calc.run()
