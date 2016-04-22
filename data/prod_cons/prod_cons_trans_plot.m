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

%% adding axis labels
ax = gca;
set(ax, 'FontName', 'Palatino Linotype', 'FontSize', 14);
xlabel('Number of Items Processed');
ylabel('Number of Transitions')
axis([0 20 10 100000000])
ax.YTick = [10 100 1000 10000 100000 1000000 10000000 100000000];

%% output pdf file
h = figure(1);
h.Units = 'centimeters'; % set figure position to cm
h.Position([3:4]) = [17,12]; % resize figure
Plot2LaTeX( h, 'prod_cons_trans_fig' )