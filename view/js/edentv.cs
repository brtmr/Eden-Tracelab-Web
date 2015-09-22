STATES = [ "#7B84E0", "#8CED87", "#E87D9C", "#F5FF85"]
ZOOM_TIMEOUT = 500
WIDTH  = 1300
HEIGHT = 700

ui_locked = off

$ ->
    $("#loading").hide()
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
        $("#loading").show()
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

    update_tracelist()

    load_machine_events_initial = () ->
        $('canvas').remove()
        $('svg').remove()
        #All trace metadata has been loaded, now load the actual trace data.
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
                        trace_loaded = true;
                        draw_machine_events(data)
                        $("#loading").hide()
                )

    calculate_minimum_duration = (start,end) ->
        lod = $("#lod").val()
        total_duration = end-start
        return Math.floor(total_duration/ WIDTH / lod)

    $("#update_button").click update_tracelist
    $("#load_button").click(() ->
            id = $("#trace_list").val()
            load_trace_info(id))

    data = dummy_data

    mk_height = (n) ->
        if (200*n)>HEIGHT then HEIGHT else 50*n

    # machine event section
    draw_machine_events = (mevents) ->
        fake = d3.behavior.zoom()

        lock_ui = () ->
            $("#loading").show()
            ui_locked = on
            canvas.call(fake)
            $("#lod").prop("disabled", true);

        unlock_ui = () ->
            canvas.call(zoom)
            $("#lod").prop("disabled", false);
            ui_locked = off
            $("#loading").hide()

        margin =
            top: 10
            right: 1
            bottom: 50
            left: 100
        width = WIDTH - margin.left - margin.right
        height = mk_height(trace_metadata.num_machines) - margin.top - margin.bottom

        x = d3.scale.linear()
            .domain( [0, trace_metadata.duration] )
            .range( [0, width] )

        tick_format = (ns) ->
            s = ns / 1000000000
            prefix =  d3.formatPrefix(s)
            return '' + s + prefix.symbol + 's'

        xAxis = d3.svg.axis().scale(x).orient("bottom").ticks(10).tickFormat(tick_format)

        timer = null

        reload = () -> 
            lock_ui()
            domain = x.domain()
            params =
                id    : trace_metadata.id
                start : Math.floor domain[0]
                end   : Math.floor domain[1]
                minduration : calculate_minimum_duration(domain[0],domain[1])
            $.post("/mevents", params, (data, status) ->
                        if status != "success"
                            alert "failed to load machine events."
                            unlock_ui()
                            return
                        mevents = data
                        draw()
                        unlock_ui()
            )

        zoomHandler = () ->
            if timer != null
                clearTimeout(timer)
            dom = x.domain()
            l = dom[1]-dom[0]
            if dom[0]<0 
                zoom.translate([0,0])
            xAxisContainer.call(xAxis)
            if ui_locked
                return
            #get the new minimum and maximum x-coordinates.
            timer = setTimeout(reload, ZOOM_TIMEOUT)
            draw()
            return

        $("#set_lod").click(() ->
            if timer != null
                clearTimeout(timer)
            timer = setTimeout(reload, ZOOM_TIMEOUT)
            )

        zoom = d3.behavior.zoom()
            .x(x)
            .scaleExtent([1, Infinity])
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

                context.fillStyle = "rgba(255,255,255,0.25)"
                context.fillRect(cos[0]-10,0,20,height + margin.top)

                context.fillStyle = "white"
                context.fillRect(cos[0]+5 ,cos[1], 120 ,-20)
                context.fillStyle = "black"
                context.font = "bold 12px sans-serif";
                context.fillText('' + Math.floor(x.invert(cos[0])+0.5) + 'ns', cos[0]+10, cos[1]-5);
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


        ## from here on drawing functions.

        barheight =
            total   : height / (trace_metadata.num_machines)
            justbar : 0.85 * height / (trace_metadata.num_machines)
            idle    : 0.2 * height / (trace_metadata.num_machines)

        drawEvent = (e) ->
            context.fillStyle = STATES[e[3]]
            context.fillRect(
                margin.left + x(e[1]),
                margin.top  + (e[0]-1) * barheight.total
                x(e[1]+e[2]) - x(e[1]),
                if e[3]==0 then barheight.idle else barheight.justbar
                )

        drawTickLine = (d) ->
            context.beginPath()
            linePos = Math.floor(margin.left + x(d)) + 0.5
            context.moveTo(linePos, 0)
            context.lineTo(linePos, height + margin.top)
            context.stroke()

        clear = () -> context.clearRect(0, 0, canvas.node().width, canvas.node().height)

        drawMachineName = (num) ->
            step = Math.floor(trace_metadata.num_machines / 32)
            if num%step != 0
                return
            context.fillStyle = "black"
            context.font = "14px sans-serif";
            context.fillText("Machine #: #{ num }", 0, num*barheight.total + margin.top);

        draw = () ->
            ticks = xAxis.scale().ticks(xAxis.ticks()[0])
            clear()
            drawEvent e for e in mevents
            drawTickLine d for d in ticks
            context.fillStyle = "white"
            context.fillRect(0,0,margin.left,margin.top + height + margin.bottom)
            drawMachineName n for n in  [1..trace_metadata.num_machines]
        draw()
    return



