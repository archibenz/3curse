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
#include <QMenuBar>
#include <QMessageBox>
#include <QPainter>
#include <QPainterPath>
#include <QPen>
#include <QPixmap>
#include <QProcess>
#include <QMenu>
#include <QString>
#include <QStringList>
#include <QTimer>
#include <QVector>
#include <QWidget>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QPushButton>

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

        setupMenus();
        setupMenuWindow();
    }

private:
    MazeWidget* viewer;
    std::unique_ptr<our::MazeSync> maze;
    QWidget* menuWindow;
    QAction* actClassic;
    QAction* actImperfect;
    QAction* actPerfectSync;
    QAction* actCustom;
    QAction* descBuild;
    QAction* descPath;
    QAction* descThreadsBuild;
    QAction* descThreadsPath;
    QAction* descSync;
    QAction* graphCompare;
    QAction* graphFixed;
    QAction* webInfo;
    QAction* exitAct;

    void setupMenus()
    {
        auto* buildMenu = menuBar()->addMenu("Построение лабиринтов");
        auto* descMenu = menuBar()->addMenu("Описание");
        auto* graphsMenu = menuBar()->addMenu("Графики");
        auto* webMenu = menuBar()->addMenu("Веб-интеграция");
        auto* exitMenu = menuBar()->addMenu("Выход");

        actClassic = buildMenu->addAction("Классический (идеальный) 20×20");
        actImperfect = buildMenu->addAction("Неидеальный (многопоточный) 20×20");
        actPerfectSync = buildMenu->addAction("Идеальный (многопоточный + синхронизация)");
        actCustom = buildMenu->addAction("Произвольный размер");

        descBuild = descMenu->addAction("Построение лабиринта");
        descPath = descMenu->addAction("Поиск кратчайшего пути");
        descThreadsBuild = descMenu->addAction("Потоки при построении");
        descThreadsPath = descMenu->addAction("Потоки при поиске пути");
        descSync = descMenu->addAction("Синхронизация (уровни и точки)");

        graphCompare = graphsMenu->addAction("1..N потоков: сравнение скорости");
        graphFixed = graphsMenu->addAction("Фикс. потоки: синхр. vs без синхр.");

        webInfo = webMenu->addAction("Инструкция по запуску Web-версии");
        exitAct = exitMenu->addAction("Выход");

        connect(actClassic, &QAction::triggered, this, [this]() {
            maze = std::make_unique<our::MazeSync>(20, 20, 1);
            maze->generate_backtrack();
            viewer->setMaze(maze.get());
            viewer->setAllPaths({});
            viewer->setExits({ maze->end_cell_cords });
            showPath();
        });
        connect(actImperfect, &QAction::triggered, this, [this]() {
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
        connect(actPerfectSync, &QAction::triggered, this, [this]() {
            bool ok=false;
            int th = QInputDialog::getInt(this,
                                          "Потоки",
                                          "Количество потоков (2-16)",
                                          4,2,16,1,&ok);
            if(!ok) return;
            maze = std::make_unique<our::MazeSync>(20,20,1);
            std::vector<std::thread> threads(th);
            std::vector<std::pair<int, int>> start_points;
            our::Thread_sync sync(&threads);
            maze->generate_and_set_random_start_end_points(start_points);
            if (start_points.size() < static_cast<std::size_t>(th)) {
                th = static_cast<int>(start_points.size());
                threads.resize(th);
            }
            for (int i = 0; i < th; ++i)
            {
                threads[i] = std::thread(&our::MazeSync::generate_multithread_backtrack,
                                         maze.get(), start_points[i], i + 1, std::ref(sync));
            }
            for (auto& t : threads) {
                if (t.joinable()) t.join();
            }
            viewer->setMaze(maze.get());
            viewer->setAllPaths({});
            viewer->setExits({ maze->end_cell_cords });
            showPath();
        });
        connect(actCustom, &QAction::triggered, this, [this](){
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

        connect(descBuild, &QAction::triggered, this, [=](){
            const char* txt =
                "Построение лабиринта (DFS-Backtracking)\n"
                "\n"
                "1) Стартуем из выбранной клетки.\n"
                "2) Выбираем случайного непосещённого соседа и ломаем стену.\n"
                "3) Добавляем клетку в стек и продолжаем, пока есть ходы.\n"
                "4) Если ходов нет — откатываемся по стеку.\n"
                "Итог: каждая клетка посещается ровно один раз, без циклов.\n";
            MainWindow::showText("Описание построения", txt);
        });
        connect(descPath, &QAction::triggered, this, [=](){
            const char* txt =
                "Поиск кратчайшего пути (BFS)\n"
                "\n"
                "1) Запускаем поиск в ширину от старта.\n"
                "2) Расширяем фронт по доступным клеткам без стен.\n"
                "3) Первое достижение выхода гарантирует кратчайший путь.\n"
                "Сложность: O(W·H) по времени и памяти.\n";
            MainWindow::showText("Описание поиска пути", txt);
        });
        connect(descThreadsBuild, &QAction::triggered, this, [=](){
            const char* txt =
                "Потоки при построении\n"
                "\n"
                "• Запускаем несколько потоков из разных стартовых точек.\n"
                "• Каждая клетка помечается владельцем потока, стены рушатся локально.\n"
                "• При встрече потоков объединяем области и продолжаем.\n";
            MainWindow::showText("Потоки при построении", txt);
        });
        connect(descThreadsPath, &QAction::triggered, this, [=](){
            const char* txt =
                "Потоки при поиске пути\n"
                "\n"
                "• Поиск пути выполняется в одном потоке (BFS).\n"
                "• Это гарантирует корректный кратчайший путь.\n"
                "• Потоковое ускорение здесь не применяется.\n";
            MainWindow::showText("Потоки при поиске пути", txt);
        });
        connect(descSync, &QAction::triggered, this, [=](){
            const char* txt =
                "Синхронизация (многоуровневая)\n"
                "\n"
                "• Локальный барьер — частая синхронизация, выравнивает прогресс\n"
                "  потоков на коротких интервалах.\n"
                "• Глобальный барьер — редкая синхронизация для общего выравнивания.\n"
                "• Барьеры переиспользуются по циклам (arrive_and_wait).\n";
            MainWindow::showText("Синхронизация", txt);
        });

        connect(graphCompare, &QAction::triggered, this, [=](){
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
            lbl->setWindowTitle("Сравнение скорости генерации (1..N)");
            lbl->setAttribute(Qt::WA_DeleteOnClose);
            lbl->show();
        });

        connect(graphFixed, &QAction::triggered, this, [=](){
            bool okT=false, okN=false;
            int th = QInputDialog::getInt(this, "Потоки", "Фиксированное число потоков", 4, 2, 64, 1, &okT);
            if(!okT) return;
            int numTests = QInputDialog::getInt(this, "Повторов",
                                                "Сколько тестов на точку (5-100)?",
                                                5, 5, 100, 1, &okN);
            if(!okN) return;

            our::test_generation_time_fixed_threads(20, 20, th, numTests, 1);
            const QString outputDir = "outputs";
            QDir().mkpath(outputDir);
            QString csvName = QString("test_generation_time_fixed_threads_%1_1mutex_cell_size.csv").arg(th);
            QString csv = outputDir + "/" + csvName;
            QString png = outputDir + "/speed_plot_fixed.png";
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
            lbl->setWindowTitle("Сравнение синхронизации (фикс. потоки)");
            lbl->setAttribute(Qt::WA_DeleteOnClose);
            lbl->show();
        });

        connect(webInfo, &QAction::triggered, this, [=](){
            const char* txt =
                "Web-интеграция\n"
                "\n"
                "Запуск:\n"
                "1) Перейдите в web/backend\n"
                "2) javac src/Main.java\n"
                "3) java -cp src Main\n"
                "4) Откройте http://localhost:8080\n";
            MainWindow::showText("Web-интеграция", txt);
        });

        connect(exitAct, &QAction::triggered, qApp, &QApplication::quit);
    }

    void setupMenuWindow()
    {
        menuWindow = new QWidget;
        menuWindow->setWindowTitle("Меню");
        auto* outer = new QVBoxLayout(menuWindow);

        auto* buildBox = new QGroupBox("Построение лабиринтов");
        auto* buildLay = new QVBoxLayout(buildBox);
        auto* btnClassic = new QPushButton("Классический (идеальный) 20×20");
        auto* btnImperfect = new QPushButton("Неидеальный (многопоточный) 20×20");
        auto* btnPerfectSync = new QPushButton("Идеальный (многопоточный + синхронизация)");
        auto* btnCustom = new QPushButton("Произвольный размер");
        buildLay->addWidget(btnClassic);
        buildLay->addWidget(btnImperfect);
        buildLay->addWidget(btnPerfectSync);
        buildLay->addWidget(btnCustom);
        buildBox->setLayout(buildLay);

        auto* descBox = new QGroupBox("Описание");
        auto* descLay = new QVBoxLayout(descBox);
        auto* btnDescBuild = new QPushButton("Построение лабиринта");
        auto* btnDescPath = new QPushButton("Поиск кратчайшего пути");
        auto* btnDescThreadsBuild = new QPushButton("Потоки при построении");
        auto* btnDescThreadsPath = new QPushButton("Потоки при поиске пути");
        auto* btnDescSync = new QPushButton("Синхронизация (уровни и точки)");
        descLay->addWidget(btnDescBuild);
        descLay->addWidget(btnDescPath);
        descLay->addWidget(btnDescThreadsBuild);
        descLay->addWidget(btnDescThreadsPath);
        descLay->addWidget(btnDescSync);
        descBox->setLayout(descLay);

        auto* graphsBox = new QGroupBox("Графики");
        auto* graphsLay = new QVBoxLayout(graphsBox);
        auto* btnGraphCompare = new QPushButton("1..N потоков: сравнение скорости");
        auto* btnGraphFixed = new QPushButton("Фикс. потоки: синхр. vs без синхр.");
        graphsLay->addWidget(btnGraphCompare);
        graphsLay->addWidget(btnGraphFixed);
        graphsBox->setLayout(graphsLay);

        auto* webBox = new QGroupBox("Веб-интеграция");
        auto* webLay = new QVBoxLayout(webBox);
        auto* btnWebInfo = new QPushButton("Инструкция по запуску Web-версии");
        webLay->addWidget(btnWebInfo);
        webBox->setLayout(webLay);

        auto* exitBox = new QGroupBox("Выход");
        auto* exitLay = new QVBoxLayout(exitBox);
        auto* btnExit = new QPushButton("Выход");
        exitLay->addWidget(btnExit);
        exitBox->setLayout(exitLay);

        outer->addWidget(buildBox);
        outer->addWidget(descBox);
        outer->addWidget(graphsBox);
        outer->addWidget(webBox);
        outer->addWidget(exitBox);
        menuWindow->setLayout(outer);
        menuWindow->move(this->geometry().right() + 20, this->geometry().top());
        menuWindow->show();

        connect(btnClassic, &QPushButton::clicked, this, [this]() { actClassic->trigger(); });
        connect(btnImperfect, &QPushButton::clicked, this, [this]() { actImperfect->trigger(); });
        connect(btnPerfectSync, &QPushButton::clicked, this, [this]() { actPerfectSync->trigger(); });
        connect(btnCustom, &QPushButton::clicked, this, [this]() { actCustom->trigger(); });
        connect(btnDescBuild, &QPushButton::clicked, this, [this]() { descBuild->trigger(); });
        connect(btnDescPath, &QPushButton::clicked, this, [this]() { descPath->trigger(); });
        connect(btnDescThreadsBuild, &QPushButton::clicked, this, [this]() { descThreadsBuild->trigger(); });
        connect(btnDescThreadsPath, &QPushButton::clicked, this, [this]() { descThreadsPath->trigger(); });
        connect(btnDescSync, &QPushButton::clicked, this, [this]() { descSync->trigger(); });
        connect(btnGraphCompare, &QPushButton::clicked, this, [this]() { graphCompare->trigger(); });
        connect(btnGraphFixed, &QPushButton::clicked, this, [this]() { graphFixed->trigger(); });
        connect(btnWebInfo, &QPushButton::clicked, this, [this]() { webInfo->trigger(); });
        connect(btnExit, &QPushButton::clicked, this, [this]() { exitAct->trigger(); });
    }

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
