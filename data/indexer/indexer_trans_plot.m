%% importing data
transitions = dlmread('indexer_trans_data.csv', ',', 1, 1);
% convert 0 values to NaNs, as these don't plot
transitions(transitions==0)=nan;
threads = [1:16];

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
set(gca, 'FontName', 'Palatino Linotype');
legend('Simple', 'Stateful Simple', 'Simple with Sleep', ...
	'DPOR', 'SDPOR', 'DPOR with Sleep', 'Location','northeast');
xlabel('Number of Threads', 'fontsize', 14);
ylabel('Number of Transitions', 'fontsize', 14)