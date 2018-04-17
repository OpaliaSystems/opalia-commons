package systems.opalia.commons.control.pid;

import java.awt.*;
import java.util.ArrayList;
import javax.swing.*;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import scala.Tuple2;


public class PlotPanel
        extends JPanel {

    private final XYSeriesCollection collection;
    private final XYPlot plot;

    public PlotPanel(ArrayList<Tuple2<String, Color>> graphs) {

        this.setLayout(new GridBagLayout());
        this.setBackground(Color.white);

        collection = new XYSeriesCollection();

        JFreeChart chart =
                ChartFactory.createXYLineChart(
                        null,
                        "time in milliseconds",
                        "value",
                        collection,
                        PlotOrientation.VERTICAL,
                        true,
                        false,
                        false);

        ChartPanel chartPanel = new ChartPanel(chart);

        chartPanel.setPopupMenu(null);
        chartPanel.setRangeZoomable(false);
        chartPanel.setDomainZoomable(false);
        chartPanel.setMinimumDrawWidth(100);
        chartPanel.setMinimumDrawHeight(100);
        chartPanel.setMaximumDrawWidth(Integer.MAX_VALUE);
        chartPanel.setMaximumDrawHeight(Integer.MAX_VALUE);

        plot = (XYPlot) chart.getPlot();

        plot.getDomainAxis().setStandardTickUnits(NumberAxis.createIntegerTickUnits());
        plot.setBackgroundPaint(Color.DARK_GRAY);

        XYLineAndShapeRenderer renderer = (XYLineAndShapeRenderer) plot.getRenderer();

        for (int i = 0; i < graphs.size(); i++) {

            Tuple2<String, Color> graph = graphs.get(i);

            collection.addSeries(new XYSeries(graph._1()));
            renderer.setSeriesPaint(i, graph._2());
        }

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.weighty = 1;
        this.add(chartPanel, gbc);
    }

    public void setRangeX(double length) {

        plot.getDomainAxis().setFixedAutoRange(length);
    }

    public void setRangeY(double lower, double upper) {

        plot.getRangeAxis().setRange(lower, upper);
    }

    public void setValues(long time, double[] values) {

        setValues(time, 0, values);
    }

    public void setValues(long time, int start, double[] values) {

        for (int i = start; i < values.length && i < collection.getSeriesCount(); i++)
            collection.getSeries(i).add(time, values[i]);
    }

    public void clear() {

        for (int i = 0; i < collection.getSeriesCount(); i++)
            collection.getSeries(i).clear();
    }
}
