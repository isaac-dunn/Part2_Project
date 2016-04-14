%% importing data
transitions = dlmread('prod_cons_trans_data.csv', ',', 1, 1);
% convert 0 values to NaNs, as these don't plot
transitions(transitions==0)=nan;
threads = [1:20];

%% plotting lines

semilogy(threads, transitions(:, 1), 'ko-')
hold on
semilogy(threads, transitions(:, 2), 'kx-')
semilogy(threads, transitions(:, 3), 'ks-')
semilogy(threads, transitions(:, 4), 'bo-')
semilogy(threads, transitions(:, 5), 'bx-')
semilogy(threads, transitions(:, 6), 'bs-')
hold off
grid on

%% adding legend and axis labels
set(gca, 'FontName', 'Palatino Linotype', 'FontSize', 14);
legend({'Simple', 'Stateful Simple', 'Simple with Sleep', ...
	'DPOR', 'SDPOR', 'DPOR with Sleep \quad'}, ...
    'Location','southeast', 'Interpreter', 'latex');
xlabel('Number of Items Processed');
ylabel('Number of Transitions')

%% output pdf file
h = figure(1);
h.Units = 'centimeters'; % set figure position to cm
%h.Position(2) = [h.Position(2)-9]; % set figure position bevore resize
h.Position([3:4]) = [16,13]; % resize figure
Plot2LaTeX( h, 'prod_cons_trans_fig' )