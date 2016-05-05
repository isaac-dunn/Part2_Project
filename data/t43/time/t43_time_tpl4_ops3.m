%% importing data
transitions = dlmread('t43_time_tpl4_ops3.csv', ',', 0, 0);
ss = dlmread('ss43_time.csv', ',', 0, 0);
ssthreads = [1:7];
% convert 0 values to NaNs, as these don't plot
transitions(transitions==0)=nan;
threads = [1:9];

%% plotting lines

semilogy(threads, transitions(:, 1), 'ko-')
hold on
semilogy(ssthreads, ss, 'kx-')
semilogy(threads, transitions(:, 3), 'ks-')
semilogy(threads, transitions(:, 4), 'bo-')
semilogy(threads, transitions(:, 5), 'bx-')
semilogy(threads, transitions(:, 6), 'bs-')
semilogy(threads, transitions(:, 7), 'rx-')
semilogy(threads, transitions(:, 8), 'rs-')
hold off
grid on

%% adding axis labels
ax = gca;
set(ax, 'FontName', 'Palatino Linotype', 'FontSize', 14);
xlabel('Number of Threads');
ylabel('Execution Time (seconds)');
axis([0 20 .001 1000])
ax.XTick = [0 5 10 15 20];
ax.YTick = [0.01 0.1 1 10 100];
set(ax,'YMinorGrid','Off', 'YMinorTick','Off');

%% output pdf file
h = figure(1);
h.Units = 'centimeters'; % set figure position to cm
%h.Position(2) = [h.Position(2)-9]; % set figure position bevore resize
h.Position([3:4]) = [10,7]; % resize figure
Plot2LaTeX( h, 't43_time_tpl4_ops3' )