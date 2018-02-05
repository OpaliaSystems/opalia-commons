package systems.opalia.commons.control.pid;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Map;
import javax.swing.*;

public class EditorPanel
        extends JPanel {

    private final JTextArea scriptText;

    public EditorPanel(Map<String, String> scripts) {

        JTextArea demoText = new JTextArea();
        scriptText = new JTextArea();

        JPanel controlPanel = new JPanel();
        JScrollPane scrollPane1 = new JScrollPane(demoText);
        JScrollPane scrollPane2 = new JScrollPane(scriptText);
        JComboBox<String> cntKeys = new JComboBox<>(scripts.keySet().toArray(new String[scripts.size()]));
        JButton btnCopy = new JButton("»");

        Font font = new Font("monospaced", Font.PLAIN, 15);

        this.setLayout(new GridBagLayout());
        this.setBackground(Color.lightGray);

        controlPanel.setLayout(new GridBagLayout());
        controlPanel.setBackground(Color.lightGray);

        controlPanel.setMinimumSize(new Dimension(
                0,
                Math.max(cntKeys.getMinimumSize().height, btnCopy.getMinimumSize().height) + 8));

        controlPanel.setPreferredSize(new Dimension(
                0,
                Math.max(cntKeys.getPreferredSize().height, btnCopy.getPreferredSize().height) + 8));

        scrollPane1.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        scrollPane2.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);

        cntKeys.setSelectedIndex(0);

        btnCopy.setFont(font);

        demoText.setFont(font);
        demoText.setLineWrap(true);
        demoText.setEditable(false);
        demoText.setBackground(Color.lightGray);

        scriptText.setFont(font);
        scriptText.setLineWrap(true);

        GridBagConstraints gbc;

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(0, 0, 0, 2);
        gbc.gridx = 0;
        gbc.gridy = 0;
        controlPanel.add(cntKeys, gbc);

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(0, 2, 0, 0);
        gbc.gridx = 1;
        gbc.gridy = 0;
        controlPanel.add(btnCopy, gbc);

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(4, 4, 4, 4);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0.5;
        this.add(controlPanel, gbc);

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0.5;
        gbc.weighty = 1;
        this.add(scrollPane1, gbc);

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.gridheight = 2;
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 0.5;
        gbc.weighty = 1;
        this.add(scrollPane2, gbc);

        ActionListener actionListener = new ActionListener() {

            public void actionPerformed(ActionEvent e) {

                if (e.getSource() == cntKeys) {

                    demoText.setText(scripts.get(cntKeys.getSelectedItem()));

                } else if (e.getSource() == btnCopy) {

                    scriptText.setText(demoText.getText());
                }
            }
        };

        cntKeys.addActionListener(actionListener);
        btnCopy.addActionListener(actionListener);

        String firstScript = scripts.entrySet().iterator().next().getValue();

        demoText.setText(firstScript);
        scriptText.setText(firstScript);
    }

    public String getScript() {

        return scriptText.getText();
    }
}
