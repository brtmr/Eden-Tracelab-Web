STATES = [ "#8CED87", "#F5FF85", "#7B84E0", "#E87D9C"]

$ ->
    tracelist_data = []
    $("#update_button").click(() ->
            console.log('clicked me')
            $.post("http://localhost:3000/traces", {}, 
                (data, status) -> 
                    alert(data)
                    alert(status)))
    data = dummy_data
    console.log(data.events.length)
    margin = 
        top: 50
        right: 50
        bottom: 50
        left: 10
    width = 1300 - margin.left - margin.right
    height = 500 - margin.top - margin.bottom

    x = d3.scale.linear()
        .domain( [0, data.endtime] )
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

    barheight = 0.8 * height / data.machines

    drawEvent = (e) -> 
        context.fillStyle = STATES[e.state]
        context.fillRect(
            margin.left + x(e.start),
            margin.top + e.id_ * height / data.machines,
            x(e.end) - x(e.start),
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
        drawEvent e for e in data.events
        drawTickLine d for d in ticks

    draw()
    return



