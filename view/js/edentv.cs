STATES = [ "#8CED87", "#F5FF85", "#7B84E0", "#E87D9C"]

MACHINE_VIEW = 1
PROCESS_VIEW = 2
THREAD_VIEW  = 4

$ ->
    tracelist_data = []
    trace_metadata = {}
    trace_loaded   = false

    #loads the list of already analyzed traces from the server.
    update_tracelist = () ->
            $.post("/traces", {},
                (data, status) ->
                    if status != "success"
                        alert "failed to fetch trace list"
                        return
                    tracelist_data = data
                    populate_options()
                    )

    populate_options = ()->
        $("#trace_list").find("option").remove()
        $("#trace_list").append(
                "<option value=\"" + x.id + "\">" + x.filename + "</option>") for x in tracelist_data

    load_trace_info = (id) ->
        #get the trace metadata
        $.post("/traceinfo", { "id" : id },
            (data, status) ->
                if status != "success"
                    alert "failed to load trace metadata."
                    return
                trace_metadata.machines = data
                trace_metadata.num_machines = data.length
                #get the trace duration
                $.post("/duration", {"id" : id},
                    (dur, status) ->
                        if status != "success"
                            alert "failed to load trace metadata."
                            return
                        trace_metadata.duration = dur[0]
                        trace_metadata.id = id
                        load_machine_events_initial()
                    )
                )

    load_machine_events_initial = () -> 
        #All trace metadata has been loaded, now load the actual trace data.
        console.log('foobar')
        params = 
            id    : trace_metadata.id
            start : 0
            end   : trace_metadata.duration
            minduration : calculate_minimum_duration(0,trace_metadata.duration)
        $.post("/mevents", params, 
                (data, status)->
                        if status != "success"
                            alert "failed to load machine events."
                            return
                        draw_machine_events(data)
                )

    calculate_minimum_duration = (start,end) ->
        total_duration = end-start
        return Math.floor(total_duration/1300)

    $("#update_button").click update_tracelist
    $("#load_button").click(() -> 
            id = $("#trace_list").val()
            load_trace_info(id))

    data = dummy_data
    margin =
        top: 50
        right: 50
        bottom: 50
        left: 10
    width = 1300 - margin.left - margin.right
    height = 500 - margin.top - margin.bottom

    draw_machine_events = (mevents) ->

        x = d3.scale.linear()
            .domain( [0, trace_metadata.duration] )
            .range( [0, width] )

        xAxis = d3.svg.axis().scale(x).orient("bottom").ticks(10)

        zoomHandler = () ->
            translate = d3.event.translate[0]
            scale     = d3.event.scale
            xAxisContainer.call(xAxis)
            draw()

        zoom = d3.behavior.zoom()
            .x(x)
            .on("zoom", zoomHandler)

        canvas = d3.select("body").append("canvas")
            .attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top)
            .call(zoom)

        context = canvas.node().getContext("2d")

        canvas.on("mousemove", () ->
                cos = d3.mouse(this)
                draw()
                context.beginPath()
                context.moveTo(cos[0], 0)
                context.lineTo(cos[0], height + margin.top)
                context.stroke()
                context.fillStyle = "black"
                context.font = "bold 12px sans-serif";
                context.fillText(x.invert(cos[0]), cos[0], cos[1]);
                )

        xAxisSvg = d3.select("body").append("svg")
            .attr("width", width + margin.left + margin.right)
            .attr("height", margin.bottom)
            .append("g")

        xAxisContainer = xAxisSvg.append("g")
            .attr("class","axis")
            .attr("transform",
                    "translate(#{margin.left},0)")
            .call(xAxis)

        barheight = 0.8 * height / trace_metadata.num_machines

        drawEvent = (e) ->
            context.fillStyle = STATES[e[3]]
            console.log context.fillStyle
            context.fillRect(
                margin.left + x(e[1]),
                margin.top + e[0] * height / trace_metadata.num_machines
                x(e[2]+1),
                barheight
                )

        drawTickLine = (d) ->
            context.beginPath()
            linePos = Math.floor(margin.left + x(d)) + 0.5
            context.moveTo(linePos, 0)
            context.lineTo(linePos, height + margin.top)
            context.stroke()

        clear = () -> context.clearRect(0, 0, canvas.node().width, canvas.node().height)

        draw = () ->
            ticks = xAxis.scale().ticks(xAxis.ticks()[0])
            clear()
            drawEvent e for e in mevents
            drawTickLine d for d in ticks

        draw()
    return



