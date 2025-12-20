#include "maze_generation.h"
#include "maze_pathfinding.h"

#include <algorithm>
#include <climits>
#include <cmath>
#include <memory>
#include <vector>

#include <QApplication>
#include <QColor>
#include <QDir>
#include <QFile>
#include <QInputDialog>
#include <QLabel>
#include <QMainWindow>
#include <QMessageBox>
#include <QPainter>
#include <QPainterPath>
#include <QPen>
#include <QPixmap>
#include <QProcess>
#include <QPushButton>
#include <QString>
#include <QStringList>
#include <QTimer>
#include <QVector>
#include <QVBoxLayout>
#include <QWidget>

namespace {

class MazeWidget : public QWidget
{
public:
    explicit MazeWidget(our::MazeSync* m = nullptr, QWidget* parent = nullptr)
        : QWidget(parent), maze(m)
    {
        animationTimer.setInterval(30);
        connect(&animationTimer, &QTimer::timeout, this, [this]() {
            if (animatedPath.empty())
            {
                animationTimer.stop();
                return;
            }
            if (animatedIndex < animatedPath.size())
            {
                ++animatedIndex;
                update();
            }
            else
            {
                animationTimer.stop();
            }
        });
    }

    void setMaze(our::MazeSync* m) { maze = m; update(); }
    void setPath(const std::vector<std::pair<int,int>>& p) {
        path = p;
        animatedPath.clear();
        animatedIndex = 0;
        animationTimer.stop();
        update();
    }
    void animatePath(const std::vector<std::pair<int,int>>& p) {
        path.clear();
        animatedPath = p;
        animatedIndex = std::min<std::size_t>(1, animatedPath.size());
        animationTimer.start();
        update();
    }
    void setExits(const std::vector<std::pair<int,int>>& ex) { exits = ex; update(); }
    void setAllPaths(const std::vector<std::vector<std::pair<int,int>>>& paths) {
        allPaths = paths;
        animatedPath.clear();
        animatedIndex = 0;
        animationTimer.stop();
        update();
    }

protected:
    void paintEvent(QPaintEvent*) override
    {
        if (!maze) return;
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing, true);
        p.fillRect(rect(), QColor(20, 22, 28));
        const int cols = maze->width;
        const int rows = maze->length;
        const int cw = width()  / std::max(1, cols);
        const int ch = height() / std::max(1, rows);

        p.setPen(QPen(QColor(50, 56, 70), 1));
        for (int i = 0; i <= rows; ++i) {
            const int y = i * ch;
            p.drawLine(0, y, cols * cw, y);
        }
        for (int j = 0; j <= cols; ++j) {
            const int x = j * cw;
            p.drawLine(x, 0, x, rows * ch);
        }

        p.setPen(QPen(QColor(220, 226, 235), 2));
        for (int i = 0; i < rows; ++i)
        {
            for (int j = 0; j < cols; ++j)
            {
                unsigned char mask = maze->cell_array[i][j].wall_direction_mask;
                const int x = j * cw;
                const int y = i * ch;
                if (mask & our::Up)    p.drawLine(x, y, x + cw, y);
                if (mask & our::Down)  p.drawLine(x, y + ch, x + cw, y + ch);
                if (mask & our::Left)  p.drawLine(x, y, x, y + ch);
                if (mask & our::Right) p.drawLine(x + cw, y, x + cw, y + ch);
            }
        }

        p.fillRect(maze->start_cell_cords.second * cw, maze->start_cell_cords.first * ch,
                   cw, ch, QColor(70, 200, 120, 160));
        for (const auto& ex : exits) {
            p.fillRect(ex.second * cw, ex.first * ch,
                       cw, ch, QColor(220, 80, 80, 160));
        }

        std::vector<std::vector<std::pair<int,int>>> toDraw;
        if (!allPaths.empty()) {
            toDraw = allPaths;
        } else if (animatedPath.size() > 1) {
            std::vector<std::pair<int,int>> slice;
            slice.reserve(animatedIndex);
            for (std::size_t i = 0; i < animatedIndex && i < animatedPath.size(); ++i) {
                slice.push_back(animatedPath[i]);
            }
            if (slice.size() > 1) {
                toDraw.push_back(slice);
            }
        } else if (path.size() > 1) {
            toDraw.push_back(path);
        }
        const QVector<QColor> colors = {
            QColor(255, 215, 0),
            QColor(0, 191, 255),
            QColor(50, 205, 50),
            QColor(255, 69, 0),
            QColor(138, 43, 226),
            QColor(0, 255, 255),
            QColor(255, 0, 255),
            QColor(218, 165, 32),
            QColor(34, 139, 34),
            QColor(255, 105, 180)
        };
        if (!toDraw.empty()) {
            const auto& mainPth = toDraw[0];
            if (mainPth.size() > 1) {
                QPainterPath pp;
                const auto first = mainPth.front();
                pp.moveTo(first.second * cw + cw/2.0, first.first * ch + ch/2.0);
                for (size_t k = 1; k < mainPth.size(); ++k) {
                    const auto pt = mainPth[k];
                    pp.lineTo(pt.second * cw + cw/2.0, pt.first * ch + ch/2.0);
                }
                QPen glow(QColor(60, 140, 255, 120));
                glow.setWidth(std::max(4, std::min(cw, ch) / 3));
                glow.setCapStyle(Qt::RoundCap);
                p.setPen(glow);
                p.drawPath(pp);

                QPen pen(colors[0]);
                pen.setWidth(std::max(2, std::min(cw, ch) / 5));
                pen.setCapStyle(Qt::RoundCap);
                p.setPen(pen);
                p.drawPath(pp);
            }
            for (int idx = 1; idx < static_cast<int>(toDraw.size()); ++idx) {
                const auto& pth = toDraw[idx];
                if (pth.size() < 2) continue;
                size_t common = 0;
                while (common < pth.size() && common < toDraw[0].size()
                       && pth[common] == toDraw[0][common]) {
                    ++common;
                }
                if (common < 1) continue;
                QPen pen(colors[idx % colors.size()]);
                pen.setWidth(std::max(2, std::min(cw, ch) / 4));
                pen.setCapStyle(Qt::RoundCap);
                p.setPen(pen);
                QPainterPath pp;
                const auto start = pth[common - 1];
                pp.moveTo(start.second * cw + cw/2.0, start.first * ch + ch/2.0);
                for (size_t k = common; k < pth.size(); ++k) {
                    const auto pt = pth[k];
                    pp.lineTo(pt.second * cw + cw/2.0, pt.first * ch + ch/2.0);
                }
                p.drawPath(pp);
            }
        }

        if (!animatedPath.empty() && animatedIndex > 0) {
            const auto head = animatedPath[std::min(animatedIndex - 1, animatedPath.size() - 1)];
            const int cx = head.second * cw + cw / 2;
            const int cy = head.first * ch + ch / 2;
            const int radius = std::max(4, std::min(cw, ch) / 3);
            p.setBrush(QColor(255, 215, 90));
            p.setPen(QPen(QColor(255, 240, 160), 2));
            p.drawEllipse(QPoint(cx, cy), radius, radius);
        }
    }

private:
    our::MazeSync* maze;
    std::vector<std::pair<int,int>> path;
    std::vector<std::pair<int,int>> animatedPath;
    std::size_t animatedIndex = 0;
    QTimer animationTimer;
    std::vector<std::pair<int,int>> exits;
    std::vector<std::vector<std::pair<int,int>>> allPaths;
};

class MainWindow : public QMainWindow
{
public:
    MainWindow(QWidget* parent = nullptr)
        : QMainWindow(parent),
          viewer(new MazeWidget(nullptr, this))
    {
        setCentralWidget(viewer);
        resize(700, 700);
        setWindowTitle("Maze GUI");
#ifdef Q_OS_MAC
        // menuBar()->setNativeMenuBar(false);
#endif
        maze = std::make_unique<our::MazeSync>(20, 20, 1);
        maze->generate_backtrack();
        viewer->setMaze(maze.get());
        viewer->setExits({ maze->end_cell_cords });
        showPath();

        menuWindow = new QWidget;
        menuWindow->setWindowTitle("Меню");
        auto *lay = new QVBoxLayout(menuWindow);
        auto *btnClassic   = new QPushButton("Классический 20×20");
        auto *btnImperfect = new QPushButton("Неидеальный 20×20");
        auto *btnSimple    = new QPushButton("Произвольный размер");
        auto *btnMath      = new QPushButton("Математическое описание");
        auto *btnGraphs    = new QPushButton("Графики производительности");
        auto *btnSyncInfo  = new QPushButton("Многоуровневая синхронизация");
        auto *btnExit      = new QPushButton("Выход");
        lay->addWidget(btnClassic);
        lay->addWidget(btnImperfect);
        lay->addWidget(btnSimple);
        lay->addWidget(btnMath);
        lay->addWidget(btnGraphs);
        lay->addWidget(btnSyncInfo);
        lay->addWidget(btnExit);
        menuWindow->setLayout(lay);
        menuWindow->move(this->geometry().right() + 20, this->geometry().top());
        menuWindow->show();

        connect(btnClassic, &QPushButton::clicked, this, [this](){
            maze = std::make_unique<our::MazeSync>(20,20,1);
            maze->generate_backtrack();
            viewer->setMaze(maze.get());
            viewer->setAllPaths({});
            viewer->setExits({ maze->end_cell_cords });
            showPath();
        });
        connect(btnImperfect, &QPushButton::clicked, this, [this](){
            bool ok=false;
            int n = QInputDialog::getInt(this,"n выходов","Сколько дополнительных выходов?",3,1,50,1,&ok);
            if(!ok) return;
            int maxThreads = n+1;
            int th = QInputDialog::getInt(this,
                                          "Потоки",
                                          QString("Количество потоков (1-%1)").arg(maxThreads),
                                          maxThreads,1,maxThreads,1,&ok);
            if(!ok) return;

            maze = std::make_unique<our::MazeSync>(20,20,1);
            maze->start_cell_cords = maze->get_random_start_point();
            maze->generate_multithread_imperfect(n, th);

            auto exits = our::add_border_exits(*maze, n);
            int minDist = std::max(maze->width, maze->length) / n;
            std::vector<std::pair<int,int>> filtered;
            for (const auto& ex : exits) {
                bool good = true;
                for (const auto& prev : filtered) {
                    int d = std::abs(ex.first - prev.first)
                          + std::abs(ex.second - prev.second);
                    if (d < minDist) { good = false; break; }
                }
                if (good) filtered.push_back(ex);
            }
            while (static_cast<int>(filtered.size()) < n) {
                auto extra = our::add_border_exits(*maze, 1)[0];
                bool good = true;
                for (const auto& prev : filtered) {
                    int d = std::abs(extra.first - prev.first)
                          + std::abs(extra.second - prev.second);
                    if (d < minDist) { good = false; break; }
                }
                if (good) filtered.push_back(extra);
            }
            exits = filtered;

            std::vector<std::vector<std::pair<int,int>>> allPathsList;
            for (const auto& ex : exits) {
                auto pth = our::find_shortest_path(*maze, maze->start_cell_cords, ex);
                if (pth.empty()) {
                    int cx = maze->start_cell_cords.first, cy = maze->start_cell_cords.second;
                    int ex_r = ex.first, ex_c = ex.second;
                    while (cx < ex_r) {
                        maze->cell_array[cx][cy].wall_direction_mask &= ~our::Down;
                        maze->cell_array[cx+1][cy].wall_direction_mask &= ~our::Up;
                        ++cx;
                    }
                    while (cx > ex_r) {
                        maze->cell_array[cx][cy].wall_direction_mask &= ~our::Up;
                        maze->cell_array[cx-1][cy].wall_direction_mask &= ~our::Down;
                        --cx;
                    }
                    while (cy < ex_c) {
                        maze->cell_array[cx][cy].wall_direction_mask &= ~our::Right;
                        maze->cell_array[cx][cy+1].wall_direction_mask &= ~our::Left;
                        ++cy;
                    }
                    while (cy > ex_c) {
                        maze->cell_array[cx][cy].wall_direction_mask &= ~our::Left;
                        maze->cell_array[cx][cy-1].wall_direction_mask &= ~our::Right;
                        --cy;
                    }
                    pth = our::find_shortest_path(*maze, maze->start_cell_cords, ex);
                }
                allPathsList.push_back(pth);
            }

            std::vector<std::pair<int,int>> nearestPath, farthestPath;
            int minLen = INT_MAX, maxLen = -1;
            for (const auto& pth : allPathsList) {
                int len = static_cast<int>(pth.size());
                if (len < minLen) {
                    minLen = len;
                    nearestPath = pth;
                }
                if (len > maxLen) {
                    maxLen = len;
                    farthestPath = pth;
                }
            }

            QStringList options = { "Ближайший", "Самый дальний", "Все" };
            bool okChoice = false;
            QString choice = QInputDialog::getItem(this, "Выбор пути", "Какой путь отображать?", options, 0, false, &okChoice);
            if (!okChoice) {
                choice = options[0];
            }

            viewer->setMaze(maze.get());
            viewer->setAllPaths({});
            if (choice == options[0]) {
                maze->end_cell_cords = nearestPath.empty() ? maze->end_cell_cords : nearestPath.back();
                viewer->setPath(nearestPath);
            } else if (choice == options[1]) {
                maze->end_cell_cords = farthestPath.empty() ? maze->end_cell_cords : farthestPath.back();
                viewer->setPath(farthestPath);
            } else {
                std::sort(allPathsList.begin(), allPathsList.end(),
                          [](const auto& a, const auto& b){ return a.size() < b.size(); });
                viewer->setAllPaths(allPathsList);
                viewer->setPath({});
            }
            viewer->setExits(exits);
        });
        connect(btnSimple, &QPushButton::clicked, this, [this](){
            bool ok1=false, ok2=false;
            int w = QInputDialog::getInt(this,"Ширина","Ширина (1-50)",20,1,50,1,&ok1);
            if(!ok1) return;
            int h = QInputDialog::getInt(this,"Высота","Высота (1-50)",20,1,50,1,&ok2);
            if(!ok2) return;
            maze = std::make_unique<our::MazeSync>(h,w,1);
            maze->generate_backtrack();
            viewer->setMaze(maze.get());
            viewer->setAllPaths({});
            viewer->setExits({ maze->end_cell_cords });
            showPath();
        });
        connect(btnMath, &QPushButton::clicked, this, [=](){
            const char* txt =
                "Алгоритмы генерации и поиска пути\n"
                "\n"
                "1) Идеальный лабиринт — рекурсивный DFS-Backtracking.\n"
                "   • Храним путь в стеке, удаляя стену к случайному непосещённому соседу.\n"
                "   • Каждая клетка посещается ровно один раз — граф без циклов.\n"
                "   • Сложность:  O(W·H) по времени и памяти.\n\n"
                "2) Неидеальный лабиринт — модифицированный Backtracking.\n"
                "   • В «тупике» с шансом ≈35 % рушим стену к уже посещённой клетке —\n"
                "     образуется петля.  Повторяем, пока не получим n дополнительных\n"
                "     проходов.\n"
                "   • Потоковая версия запускает k потоков из разных углов; секции\n"
                "     mutex_cell_size×mutex_cell_size защищены одним мьютексом.\n"
                "   • Сложность остаётся O(W·H), но время падает ~1/k.\n\n"
                "3) BFS — кратчайший путь.\n"
                "   • Фронт «растёт» по клеткам без стен; первое достижение выхода даёт\n"
                "     минимальный маршрут.\n"
                "   • Сложность: O(W·H) по времени и памяти.\n";
            MainWindow::showText("Алгоритмы", txt);
        });
        connect(btnSyncInfo, &QPushButton::clicked, this, [=](){
            const char* txt =
                "Многоуровневая синхронизация потоков\n"
                "\n"
                "• Используется два барьера: локальный и глобальный.\n"
                "• Локальный барьер синхронизирует потоки чаще, чтобы держать\n"
                "  генерацию равномерной по зонам.\n"
                "• Глобальный барьер срабатывает реже и выравнивает общий прогресс.\n"
                "• Барьеры повторно используются: поток ждёт остальных, затем\n"
                "  все продолжают работу в следующем цикле.\n";
            MainWindow::showText("Синхронизация", txt);
        });
        connect(btnGraphs, &QPushButton::clicked, this, [=](){
            bool okA=false, okB=false;
            int minTh = QInputDialog::getInt(this, "Минимум потоков", "от", 2, 2, 64, 1, &okA);
            if(!okA) return;
            int maxTh = QInputDialog::getInt(this, "Максимум потоков",
                                            QString("до (≥%1)").arg(minTh),
                                            minTh, minTh, 64, 1, &okB);
            if(!okB) return;
            bool okNT = false;
            int numTests = QInputDialog::getInt(this, "Повторов",
                                                "Сколько тестов на точку (5-100)?",
                                                5, 5, 100, 1, &okNT);
            if(!okNT) return;

            our::test_generation_time_comparison(20, 20,
                                                 minTh, maxTh,
                                                 numTests, 1);
            const QString outputDir = "outputs";
            QDir().mkpath(outputDir);
            QString csvName = "test_generation_time_comparison_1mutex_cell_size.csv";
            QString csv = outputDir + "/" + csvName;
            QString png = outputDir + "/speed_plot.png";
            if (QFile::exists(csvName)) {
                QFile::remove(csv);
                QFile::rename(csvName, csv);
            }

            QProcess p;
            p.start("python3", {"scripts/gen_speed_plot.py", csv, png});
            p.waitForFinished(-1);

            if(!QFile::exists(png)) {
                MainWindow::showText("Ошибка",
                    "PNG не создан. Убедитесь, что установлен Python 3 + matplotlib.");
                return;
            }
            QLabel* lbl = new QLabel;
            lbl->setPixmap(QPixmap(png));
            lbl->setWindowTitle("Сравнение скорости генерации");
            lbl->setAttribute(Qt::WA_DeleteOnClose);
            lbl->show();
        });
        connect(btnExit, &QPushButton::clicked, qApp, &QApplication::quit);
    }

private:
    MazeWidget* viewer;
    std::unique_ptr<our::MazeSync> maze;
    QWidget* menuWindow;

    void showPath()
    {
        auto route = our::find_shortest_path(*maze,
                                             maze->start_cell_cords,
                                             maze->end_cell_cords);
        viewer->animatePath(route);
        if(route.empty()) viewer->setExits({});
    }

    static void showText(const QString& title, const QString& body)
    {
        QMessageBox msg;
        msg.setIcon(QMessageBox::NoIcon);
        msg.setWindowTitle(title);
        msg.setTextFormat(Qt::PlainText);
        msg.setText(body);
        msg.exec();
    }
};

} // namespace

int main(int argc, char* argv[])
{
    QApplication app(argc, argv);
    MainWindow w;
    w.show();
    return app.exec();
}
