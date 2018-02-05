package systems.opalia.commons.control.pid;

import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.concurrent.ScheduledExecutorService;
import java.util.function.Consumer;
import javax.swing.*;
import scala.Tuple2;
import systems.opalia.commons.scripting.calculator.Calculator;
import systems.opalia.commons.scripting.calculator.FunctionApp;
import systems.opalia.commons.scripting.calculator.FunctionDef;


public class Dialog
        extends JFrame {

    public Dialog(ScheduledExecutorService scheduler,
                  Configuration config,
                  Calculator calculator) {

        super("PID Controller Test");

        ArrayList<Tuple2<String, Color>> graphs = new ArrayList<>();

        graphs.add(new Tuple2<>("Setpoint", Color.ORANGE));
        graphs.add(new Tuple2<>("Input", Color.GREEN));
        graphs.add(new Tuple2<>("Output", Color.BLUE));

        JPanel container = new JPanel();
        JTabbedPane tabbedPane = new JTabbedPane();
        PlotPanel plotPanel = new PlotPanel(graphs);
        EditorPanel generatorEditorPanel = new EditorPanel(config.generatorScripts());
        EditorPanel feedbackEditorPanel = new EditorPanel(config.feedbackScripts());

        ConsolePanel consolePanel =
                new ConsolePanel(this, config, calculator, generatorEditorPanel, feedbackEditorPanel, plotPanel);

        plotPanel.setRangeX(20000);
        plotPanel.setRangeY(-2.0, 2.0);

        config.setPlottingMethod((tuple) -> {

            double[] values = new double[]{tuple._2(), tuple._3(), tuple._4()};

            plotPanel.setValues(tuple._1(), values);
        });

        applyCalculatorFunction(
                calculator,
                generatorEditorPanel,
                config::setGeneratorFunction,
                config.generatorSignature());

        applyCalculatorFunction(
                calculator,
                feedbackEditorPanel,
                config::setFeedbackFunction,
                config.feedbackSignature());

        container.setLayout(new GridBagLayout());
        container.setBackground(Color.white);
        tabbedPane.setBackground(Color.white);

        GridBagConstraints gbc;

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.insets = new Insets(6, 3, 3, 4);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weighty = 1;
        container.add(consolePanel, gbc);

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.insets = new Insets(6, 4, 3, 3);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.weighty = 1;
        container.add(tabbedPane, gbc);

        tabbedPane.addTab("Plot", plotPanel);
        tabbedPane.addTab("Generator Function", generatorEditorPanel);
        tabbedPane.addTab("Feedback Function", feedbackEditorPanel);

        add(container);

        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        setExtendedState(getExtendedState() | JFrame.MAXIMIZED_BOTH);

        addWindowListener(new WindowAdapter() {

            public void windowClosing(WindowEvent e) {

                setVisible(false);
                scheduler.shutdown();
            }
        });

        pack();
        setLocationRelativeTo(null);
        setVisible(true);
    }

    void handleError(Runnable f, Runnable g) {

        boolean ok = false;

        try {

            f.run();
            ok = true;

        } catch (Throwable e) {

            JOptionPane.showMessageDialog(
                    Dialog.this,
                    e.getMessage(),
                    e.getClass().getSimpleName(),
                    JOptionPane.ERROR_MESSAGE);
        }

        if (ok)
            g.run();
    }

    void applyCalculatorFunction(Calculator calculator,
                                 EditorPanel panel,
                                 Consumer<FunctionApp> consumer,
                                 FunctionDef.Signature signature) {

        FunctionApp[] app = {null};

        handleError(
                () -> app[0] = calculator.bindFunction(panel.getScript(), signature),
                () -> consumer.accept(app[0]));
    }
}
