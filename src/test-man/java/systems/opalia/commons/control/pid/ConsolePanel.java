package systems.opalia.commons.control.pid;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;
import systems.opalia.commons.scripting.calculator.Calculator;

public class ConsolePanel
        extends JPanel {

    public ConsolePanel(Dialog dialog,
                        Configuration config,
                        Calculator calculator,
                        EditorPanel generatorEditorPanel,
                        EditorPanel feedbackEditorPanel,
                        PlotPanel plotPanel) {

        this.setLayout(new GridBagLayout());
        this.setBackground(Color.white);

        this.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createTitledBorder("Console"),
                BorderFactory.createEmptyBorder(5, 5, 5, 5)));

        this.setMinimumSize(new Dimension(240, 240));
        this.setPreferredSize(new Dimension(240, 240));

        GridBagConstraints gbc;

        JLabel lblPlotSettings = new JLabel("Plot Settings", SwingConstants.CENTER);
        lblPlotSettings.setOpaque(true);
        lblPlotSettings.setBackground(Color.lightGray);

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.insets = new Insets(14, 2, 4, 2);
        this.add(lblPlotSettings, gbc);

        JLabel lblPlotDomainAxisLength = new JLabel("Domain Axis Length");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 2, 1, 1);
        this.add(lblPlotDomainAxisLength, gbc);

        JTextField cntPlotDomainAxisLength = new JTextField();

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 1, 1, 2);
        this.add(cntPlotDomainAxisLength, gbc);

        JLabel lblPlotRangeAxisMaximum = new JLabel("Range Axis Maximum");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 2, 1, 1);
        this.add(lblPlotRangeAxisMaximum, gbc);

        JTextField cntPlotRangeAxisMaximum = new JTextField();

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 1, 1, 2);
        this.add(cntPlotRangeAxisMaximum, gbc);

        JLabel lblPlotRangeAxisMinimum = new JLabel("Range Axis Minimum");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 2, 1, 1);
        this.add(lblPlotRangeAxisMinimum, gbc);

        JTextField cntPlotRangeAxisMinimum = new JTextField();

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 1, 1, 2);
        this.add(cntPlotRangeAxisMinimum, gbc);

        JButton btnPlotApplySettings = new JButton("Apply Settings");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.insets = new Insets(2, 2, 2, 2);
        this.add(btnPlotApplySettings, gbc);

        JLabel lblPidSettings = new JLabel("PID Settings", SwingConstants.CENTER);
        lblPidSettings.setOpaque(true);
        lblPidSettings.setBackground(Color.lightGray);

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.weightx = 1;
        gbc.insets = new Insets(14, 2, 4, 2);
        this.add(lblPidSettings, gbc);

        JLabel lblPidProportionalGain = new JLabel("Proportional Gain");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 6;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 2, 1, 1);
        this.add(lblPidProportionalGain, gbc);

        JTextField cntPidProportionalGain = new JTextField();

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        gbc.gridy = 6;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 1, 1, 2);
        this.add(cntPidProportionalGain, gbc);

        JLabel lblPidIntegralGain = new JLabel("Integral Gain");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 7;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 2, 1, 1);
        this.add(lblPidIntegralGain, gbc);

        JTextField cntPidIntegralGain = new JTextField();

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        gbc.gridy = 7;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 1, 1, 2);
        this.add(cntPidIntegralGain, gbc);

        JLabel lblPidDerivativeGain = new JLabel("Derivative Gain");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 8;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 2, 1, 1);
        this.add(lblPidDerivativeGain, gbc);

        JTextField cntPidDerivativeGain = new JTextField();

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        gbc.gridy = 8;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 1, 1, 2);
        this.add(cntPidDerivativeGain, gbc);

        JLabel lblPidFilterCoefficient = new JLabel("Filter Coefficient");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 9;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 2, 1, 1);
        this.add(lblPidFilterCoefficient, gbc);

        JTextField cntPidFilterCoefficient = new JTextField();

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        gbc.gridy = 9;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 1, 1, 2);
        this.add(cntPidFilterCoefficient, gbc);

        JLabel lblPidProportionalOnMeasurement = new JLabel("P. On Measurement");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 10;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 2, 1, 1);
        this.add(lblPidProportionalOnMeasurement, gbc);

        JCheckBox cntPidProportionalOnMeasurement = new JCheckBox();
        cntPidProportionalOnMeasurement.setBackground(Color.white);

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        gbc.gridy = 10;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 1, 1, 2);
        this.add(cntPidProportionalOnMeasurement, gbc);

        JLabel lblPidInverse = new JLabel("Inverse");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 11;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 2, 1, 1);
        this.add(lblPidInverse, gbc);

        JCheckBox cntPidInverse = new JCheckBox();
        cntPidInverse.setBackground(Color.white);

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        gbc.gridy = 11;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 1, 1, 2);
        this.add(cntPidInverse, gbc);

        JLabel lblPidOutputMaximum = new JLabel("Output Maximum");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 12;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 2, 1, 1);
        this.add(lblPidOutputMaximum, gbc);

        JTextField cntPidOutputMaximum = new JTextField();

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        gbc.gridy = 12;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 1, 1, 2);
        this.add(cntPidOutputMaximum, gbc);

        JLabel lblPidOutputMinimum = new JLabel("Output Minimum");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 13;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 2, 1, 1);
        this.add(lblPidOutputMinimum, gbc);

        JTextField cntPidOutputMinimum = new JTextField();

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        gbc.gridy = 13;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 1, 1, 2);
        this.add(cntPidOutputMinimum, gbc);

        JButton btnPidApplySettings = new JButton("Apply Settings");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy = 14;
        gbc.weightx = 1;
        gbc.insets = new Insets(2, 2, 2, 2);
        this.add(btnPidApplySettings, gbc);

        JToggleButton btnPidRun = new JToggleButton("Run");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy = 15;
        gbc.weightx = 1;
        gbc.insets = new Insets(2, 2, 2, 2);
        this.add(btnPidRun, gbc);

        JButton btnPidReset = new JButton("Reset");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy = 16;
        gbc.weightx = 1;
        gbc.insets = new Insets(2, 2, 2, 2);
        this.add(btnPidReset, gbc);

        JButton btnPidInitialize = new JButton("Initialize");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy = 17;
        gbc.weightx = 1;
        gbc.insets = new Insets(2, 2, 2, 2);
        this.add(btnPidInitialize, gbc);

        JLabel lblSignalSettings = new JLabel("Signal Settings", SwingConstants.CENTER);
        lblSignalSettings.setOpaque(true);
        lblSignalSettings.setBackground(Color.lightGray);

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy = 18;
        gbc.weightx = 1;
        gbc.insets = new Insets(14, 2, 4, 2);
        this.add(lblSignalSettings, gbc);

        JLabel lblSignalCalculationSkip = new JLabel("Calculation Skip");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 19;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 2, 1, 1);
        this.add(lblSignalCalculationSkip, gbc);

        JTextField cntSignalCalculationSkip = new JTextField();

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        gbc.gridy = 19;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 1, 1, 2);
        this.add(cntSignalCalculationSkip, gbc);

        JLabel lblSignalSampleTime = new JLabel("Sample Time");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 20;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 2, 1, 1);
        this.add(lblSignalSampleTime, gbc);

        JTextField cntSignalSampleTime = new JTextField();

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        gbc.gridy = 20;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 1, 1, 2);
        this.add(cntSignalSampleTime, gbc);

        JLabel lblSignalOffset = new JLabel("Offset");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 21;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 2, 1, 1);
        this.add(lblSignalOffset, gbc);

        JTextField cntSignalOffset = new JTextField();

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        gbc.gridy = 21;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 1, 1, 2);
        this.add(cntSignalOffset, gbc);

        JLabel lblSignalFactor = new JLabel("Factor");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 22;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 2, 1, 1);
        this.add(lblSignalFactor, gbc);

        JTextField cntSignalFactor = new JTextField();

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        gbc.gridy = 22;
        gbc.weightx = 1;
        gbc.insets = new Insets(1, 1, 1, 2);
        this.add(cntSignalFactor, gbc);

        JToggleButton btnSignalRun = new JToggleButton("Run");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy = 23;
        gbc.weightx = 1;
        gbc.insets = new Insets(2, 2, 2, 2);
        this.add(btnSignalRun, gbc);

        JButton btnSignalSetGeneratorFunction = new JButton("Set Generator Function");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy = 24;
        gbc.weightx = 1;
        gbc.insets = new Insets(2, 2, 2, 2);
        this.add(btnSignalSetGeneratorFunction, gbc);

        JButton btnSignalSetFeedbackFunction = new JButton("Set Feedback Function");

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy = 25;
        gbc.weightx = 1;
        gbc.insets = new Insets(2, 2, 2, 2);
        this.add(btnSignalSetFeedbackFunction, gbc);

        JPanel pnlSpace = new JPanel();
        pnlSpace.setBackground(Color.white);

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy = 26;
        gbc.weightx = 1;
        gbc.weighty = 1;
        this.add(pnlSpace, gbc);

        ActionListener actionListener = new ActionListener() {

            String regexInteger = "[+-]?(0|([1-9][0-9]*))";
            String regexFloat = "[+-]?(0|([1-9][0-9]*))(\\.[0-9]+)?";

            public void actionPerformed(ActionEvent event) {

                if (event.getSource() == btnPlotApplySettings) {

                    String textDomainAxisLength = cntPlotDomainAxisLength.getText();
                    String textRangeAxisMaximum = cntPlotRangeAxisMaximum.getText();
                    String textRangeAxisMinimum = cntPlotRangeAxisMinimum.getText();

                    dialog.handleError(
                            () -> {

                                if (!textDomainAxisLength.matches(regexInteger))
                                    throw new IllegalArgumentException(
                                            "Cannot parse value for domain axis length.");

                                if (!(Long.parseLong(textDomainAxisLength) > 0))
                                    throw new IllegalArgumentException(
                                            "Value for domain axis length must be greater than 0.");

                                if (!textRangeAxisMaximum.matches(regexFloat))
                                    throw new IllegalArgumentException(
                                            "Cannot parse value for range axis maximum.");

                                if (!textRangeAxisMinimum.matches(regexFloat))
                                    throw new IllegalArgumentException(
                                            "Cannot parse value for range axis minimum.");

                                if (!(Double.parseDouble(textRangeAxisMaximum) >
                                        Double.parseDouble(textRangeAxisMinimum)))
                                    throw new IllegalArgumentException(
                                            "Values for range axis are not a valid interval.");
                            },
                            () -> {

                                plotPanel.setRangeX(Long.parseLong(textDomainAxisLength));

                                plotPanel.setRangeY(
                                        Double.parseDouble(textRangeAxisMinimum),
                                        Double.parseDouble(textRangeAxisMaximum));
                            });

                } else if (event.getSource() == btnSignalRun) {

                    boolean run = btnSignalRun.isSelected();

                    String textCalculationSkip = cntSignalCalculationSkip.getText();
                    String textSampleTime = cntSignalSampleTime.getText();
                    String textOffset = cntSignalOffset.getText();
                    String textFactor = cntSignalFactor.getText();

                    if (run) {

                        dialog.handleError(
                                () -> {

                                    if (!textCalculationSkip.matches(regexInteger))
                                        throw new IllegalArgumentException(
                                                "Cannot parse value for calculation skip.");

                                    if (Long.parseLong(textCalculationSkip) < 0)
                                        throw new IllegalArgumentException(
                                                "Value for calculation skip must be positive.");

                                    if (!textSampleTime.matches(regexInteger))
                                        throw new IllegalArgumentException(
                                                "Cannot parse value for sample time.");

                                    if (!(Long.parseLong(textSampleTime) > 0))
                                        throw new IllegalArgumentException(
                                                "Value for sample time must be greater than 0.");

                                    if (!textOffset.matches(regexFloat))
                                        throw new IllegalArgumentException(
                                                "Cannot parse value for offset.");

                                    if (!textFactor.matches(regexFloat))
                                        throw new IllegalArgumentException(
                                                "Cannot parse value for factor.");
                                },
                                () -> {

                                    plotPanel.clear();

                                    config.startSignalCalculation(
                                            Long.parseLong(textCalculationSkip),
                                            Long.parseLong(textSampleTime),
                                            Double.parseDouble(textOffset),
                                            Double.parseDouble(textFactor));
                                });

                    } else {

                        config.stopSignalCalculation();
                    }

                } else if (event.getSource() == btnSignalSetGeneratorFunction) {

                    dialog.applyCalculatorFunction(
                            calculator,
                            generatorEditorPanel,
                            config::setGeneratorFunction,
                            config.generatorSignature());

                } else if (event.getSource() == btnSignalSetFeedbackFunction) {

                    dialog.applyCalculatorFunction(
                            calculator,
                            feedbackEditorPanel,
                            config::setFeedbackFunction,
                            config.feedbackSignature());

                } else if (event.getSource() == btnPidApplySettings) {

                    String textProportionalGain = cntPidProportionalGain.getText();
                    String textIntegralGain = cntPidIntegralGain.getText();
                    String textDerivativeGain = cntPidDerivativeGain.getText();
                    String textFilterCoefficient = cntPidFilterCoefficient.getText();
                    boolean textProportionalOnMeasurement = cntPidProportionalOnMeasurement.isSelected();
                    boolean textInverse = cntPidInverse.isSelected();
                    String textOutputMaximum = cntPidOutputMaximum.getText();
                    String textOutputMinimum = cntPidOutputMinimum.getText();

                    PidController.Tuning[] tuning = {null};
                    PidController.OutputRange[] outputRange = {null};

                    dialog.handleError(
                            () -> {

                                if (!textProportionalGain.matches(regexFloat))
                                    throw new IllegalArgumentException(
                                            "Cannot parse value for proportional gain.");

                                if (!textIntegralGain.matches(regexFloat))
                                    throw new IllegalArgumentException(
                                            "Cannot parse value for integral gain.");

                                if (!textDerivativeGain.matches(regexFloat))
                                    throw new IllegalArgumentException(
                                            "Cannot parse value for derivative gain.");

                                if (!textFilterCoefficient.matches(regexFloat))
                                    throw new IllegalArgumentException(
                                            "Cannot parse value for filter coefficient.");

                                if (!textOutputMaximum.matches(regexFloat))
                                    throw new IllegalArgumentException(
                                            "Cannot parse value for output maximum.");

                                if (!textOutputMinimum.matches(regexFloat))
                                    throw new IllegalArgumentException(
                                            "Cannot parse value for output minimum.");

                                tuning[0] = new PidController.Tuning(
                                        Double.parseDouble(textProportionalGain),
                                        Double.parseDouble(textIntegralGain),
                                        Double.parseDouble(textDerivativeGain),
                                        Double.parseDouble(textFilterCoefficient),
                                        textProportionalOnMeasurement,
                                        textInverse);

                                outputRange[0] = new PidController.OutputRange(
                                        Double.parseDouble(textOutputMaximum),
                                        Double.parseDouble(textOutputMinimum));
                            },
                            () -> {

                                config.setPidTuning(tuning[0]);
                                config.setPidOutputRange(outputRange[0]);
                            });

                } else if (event.getSource() == btnPidRun) {

                    config.runPid(btnPidRun.isSelected());

                } else if (event.getSource() == btnPidReset) {

                    config.resetPid();

                } else if (event.getSource() == btnPidInitialize) {

                    config.initializePid();
                }
            }
        };

        cntPlotDomainAxisLength.setText(Long.toString(20000));
        cntPlotRangeAxisMaximum.setText(Double.toString(2));
        cntPlotRangeAxisMinimum.setText(Double.toString(-2));
        cntPidProportionalGain.setText(Double.toString(config.getPidTuning().proportionalGain()));
        cntPidIntegralGain.setText(Double.toString(config.getPidTuning().integralGain()));
        cntPidDerivativeGain.setText(Double.toString(config.getPidTuning().derivativeGain()));
        cntPidFilterCoefficient.setText(Double.toString(config.getPidTuning().filterCoefficient()));
        cntPidProportionalOnMeasurement.setSelected(config.getPidTuning().proportionalOnMeasurement());
        cntPidInverse.setSelected(config.getPidTuning().inverse());
        cntPidOutputMaximum.setText(Double.toString(config.getPidOutputRange().maximum()));
        cntPidOutputMinimum.setText(Double.toString(config.getPidOutputRange().minimum()));
        cntSignalCalculationSkip.setText(Long.toString(0));
        cntSignalSampleTime.setText(Long.toString(100));
        cntSignalOffset.setText(Double.toString(0));
        cntSignalFactor.setText(Double.toString(1));

        btnPlotApplySettings.addActionListener(actionListener);
        btnPidApplySettings.addActionListener(actionListener);
        btnPidRun.addActionListener(actionListener);
        btnPidReset.addActionListener(actionListener);
        btnPidInitialize.addActionListener(actionListener);
        btnSignalRun.addActionListener(actionListener);
        btnSignalSetGeneratorFunction.addActionListener(actionListener);
        btnSignalSetFeedbackFunction.addActionListener(actionListener);
    }
}
