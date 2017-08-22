%% importing data
transitions = dlmread('indexer_time_data.csv', ',', 1, 1);
ss = dlmread('ss.csv', ',', 0, 0);
% convert 0 values to NaNs, as these don't plot
transitions(transitions==0)=nan;
threads = [1:17];
ssthreads = [1:3];

%% plotting lines

semilogy(threads, transitions(:, 1), 'ko-')
hold on
semilogy(ssthreads, ss(:, 2), 'kx-')
semilogy(threads, transitions(:, 3), 'ks-')
semilogy(threads, transitions(:, 4), 'bo-')
semilogy(threads, transitions(:, 5), 'bx-')
semilogy(threads, transitions(:, 6), 'bs-')
hold off
grid on

%% adding axis labels
set(gca, 'FontName', 'Palatino Linotype', 'FontSize', 14);
xlabel('Number of Threads');
ylabel('Execution Time (seconds)')

%% output pdf file
h = figure(1);
h.Units = 'centimeters'; % set figure position to cm
h.Position([3:4]) = [17,12]; % resize figure
Plot2LaTeX( h, 'indexer_time_fig' )