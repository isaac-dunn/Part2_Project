%% create legend only
f = openfig('all_legend');
axis off
axesHandlesToChildObjects = findobj(gca, 'Type', 'line');
delete(axesHandlesToChildObjects);
leg=legend;
leg.Position = [0.5 0.5 0 0];

%% print
print(f, 'key','-dpdf')
set(f,'PaperSize',[15, 5])
print(f,'key','-dpdf','-r0')
